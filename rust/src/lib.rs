use base64::{engine::general_purpose, Engine as _};
use openpgp::{
    cert::Cert,
    crypto::SessionKey,
    packet::{PKESK, SKESK},
    parse::{stream::*, Parse},
    policy::{Policy, StandardPolicy},
    serialize::stream::{Encryptor2, LiteralWriter, Message},
    types::SymmetricAlgorithm,
};
use sequoia_openpgp as openpgp;
use std::ffi::CString;
use std::io::Cursor;
use std::io::{Read, Write};
use std::os::raw::c_char;
use thiserror::Error;

pub fn encrypt_payload<R: Read, W: Write + Send + Sync>(
    mut input: R,
    output: W,
    recipient_cert: &Cert,
) -> Result<(), CryptoError> {
    let policy = &StandardPolicy::new();

    let recipients = recipient_cert
        .keys()
        .with_policy(policy, None)
        .supported()
        .alive()
        .for_transport_encryption()
        .collect::<Vec<_>>();

    if recipients.is_empty() {
        return Err(CryptoError::NoEncryptionKey);
    }

    let message = Message::new(output);

    let encryptor = Encryptor2::for_recipients(message, recipients)
        .build()
        .map_err(|e| {
            CryptoError::Io(std::io::Error::new(
                std::io::ErrorKind::Other,
                e.to_string(),
            ))
        })?;

    let mut literal_writer = LiteralWriter::new(encryptor).build().map_err(|e| {
        CryptoError::Io(std::io::Error::new(
            std::io::ErrorKind::Other,
            e.to_string(),
        ))
    })?;

    std::io::copy(&mut input, &mut literal_writer)?;
    literal_writer.finalize().map_err(|e| {
        CryptoError::Io(std::io::Error::new(
            std::io::ErrorKind::Other,
            e.to_string(),
        ))
    })?;

    Ok(())
}

pub fn decrypt_payload<R: Read + Send + Sync, W: Write>(
    input: R,
    mut output: W,
    private_cert: &Cert,
) -> Result<(), CryptoError> {
    let policy = &StandardPolicy::new();

    let helper = DecryptionHelperImpl {
        cert: private_cert,
        policy,
    };

    let mut decryptor = DecryptorBuilder::from_reader(input)
        .map_err(|e| {
            CryptoError::Io(std::io::Error::new(
                std::io::ErrorKind::Other,
                e.to_string(),
            ))
        })?
        .with_policy(policy, None, helper)
        .map_err(|e| {
            CryptoError::Io(std::io::Error::new(
                std::io::ErrorKind::Other,
                e.to_string(),
            ))
        })?;

    std::io::copy(&mut decryptor, &mut output)?;
    Ok(())
}

#[derive(Debug, Error)]
pub enum CryptoError {
    #[error("OpenPGP error: {0}")]
    Pgp(#[from] sequoia_openpgp::Error),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("No suitable encryption key found")]
    NoEncryptionKey,

    #[error("No suitable decryption key found")]
    NoDecryptionKey,
}

struct DecryptionHelperImpl<'a> {
    cert: &'a Cert,
    policy: &'a dyn Policy,
}

impl<'a> VerificationHelper for DecryptionHelperImpl<'a> {
    fn get_certs(&mut self, _ids: &[openpgp::KeyHandle]) -> openpgp::Result<Vec<Cert>> {
        Ok(vec![])
    }

    fn check(&mut self, _structure: MessageStructure) -> openpgp::Result<()> {
        Ok(())
    }
}

impl<'a> DecryptionHelper for DecryptionHelperImpl<'a> {
    fn decrypt<D>(
        &mut self,
        pkesks: &[PKESK],
        _skesks: &[SKESK],
        _sym_algo: Option<SymmetricAlgorithm>,
        mut decrypt: D,
    ) -> openpgp::Result<Option<openpgp::Fingerprint>>
    where
        D: FnMut(SymmetricAlgorithm, &SessionKey) -> bool,
    {
        for pkesk in pkesks {
            if let Some(key) = self
                .cert
                .keys()
                .with_policy(self.policy, None)
                .secret()
                .for_transport_encryption()
                .find(|k| k.key().keyid() == *pkesk.recipient())
            {
                let mut keypair = key.key().clone().into_keypair()?;

                if let Some((algo, session_key)) = pkesk.decrypt(&mut keypair, _sym_algo) {
                    if decrypt(algo, &session_key) {
                        return Ok(Some(key.key().fingerprint()));
                    }
                }
            }
        }
        Ok(None)
    }
}

/// Returns encrypted PGP bytes as C string (caller must free with free_string)
#[no_mangle]
pub extern "C" fn encrypt_data(
    key_data: *const u8,
    key_len: usize,
    plaintext_data: *const u8,
    plaintext_len: usize,
) -> *mut c_char {
    if key_data.is_null() || plaintext_data.is_null() {
        return std::ptr::null_mut();
    }

    unsafe {
        let key_bytes = std::slice::from_raw_parts(key_data, key_len);
        let plaintext_bytes = std::slice::from_raw_parts(plaintext_data, plaintext_len);

        // Load the certificate
        let cert = match Cert::from_bytes(key_bytes) {
            Ok(c) => {
                eprintln!("Certificate loaded successfully");
                c
            }
            Err(e) => {
                eprintln!("Certificate parsing failed: {}", e);
                return std::ptr::null_mut();
            }
        };

        // Encrypt
        let mut encrypted = Vec::new();
        match encrypt_payload(Cursor::new(plaintext_bytes), &mut encrypted, &cert) {
            Ok(_) => {
                // Encode binary data as base64 to avoid null bytes
                let encoded = general_purpose::STANDARD.encode(&encrypted);
                match CString::new(encoded) {
                    Ok(s) => s.into_raw(),
                    Err(e) => {
                        eprintln!("CString conversion failed: {}", e);
                        std::ptr::null_mut()
                    }
                }
            }
            Err(e) => {
                eprintln!("Encryption failed: {}", e);
                std::ptr::null_mut()
            }
        }
    }
}

/// Returns decrypted plaintext as C string (caller must free with free_string)
#[no_mangle]
pub extern "C" fn decrypt_data(
    key_data: *const u8,
    key_len: usize,
    encrypted_data: *const u8,
    encrypted_len: usize,
) -> *mut c_char {
    if key_data.is_null() || encrypted_data.is_null() {
        return std::ptr::null_mut();
    }

    unsafe {
        let key_bytes = std::slice::from_raw_parts(key_data, key_len);
        let encrypted_bytes = std::slice::from_raw_parts(encrypted_data, encrypted_len);

        // Load the certificate
        let cert = match Cert::from_bytes(key_bytes) {
            Ok(c) => c,
            Err(_) => return std::ptr::null_mut(),
        };

        // Decrypt
        let mut decrypted = Vec::new();
        match decrypt_payload(Cursor::new(encrypted_bytes), &mut decrypted, &cert) {
            Ok(_) => {
                // Decrypted data is plaintext, convert to string
                match String::from_utf8(decrypted) {
                    Ok(s) => match CString::new(s) {
                        Ok(cs) => cs.into_raw(),
                        Err(e) => {
                            eprintln!("CString conversion failed: {}", e);
                            std::ptr::null_mut()
                        }
                    },
                    Err(e) => {
                        eprintln!("UTF-8 conversion failed: {}", e);
                        std::ptr::null_mut()
                    }
                }
            }
            Err(e) => {
                eprintln!("Decryption failed: {}", e);
                std::ptr::null_mut()
            }
        }
    }
}

/// Free C string allocated by Rust
#[no_mangle]
pub extern "C" fn free_string(s: *mut c_char) {
    if !s.is_null() {
        unsafe {
            drop(CString::from_raw(s));
        }
    }
}
