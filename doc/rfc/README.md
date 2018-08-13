# Relevant IETF RFCs

## Elliptic Curves for Security

Informational [RFC 7748](rfc7748.txt) describes the alternative curves
`X25519` and `X448` for `ECDH`.

## Edwards-Curve Digital Signature Algorithm (EdDSA)

Informational [RFC 8032](rfc8032.txt) describes the signature
algorithm `EdDSA`.

## Elliptic Curve Cryptography (ECC) Cipher Suites for Transport Layer Security (TLS) Versions 1.2 and Earlier

Proposed Standard [RFC 8422](rfc8422.txt) extends the TLS spec with
`ECDSA` and `EdDSA` for certificate signing and restricts `ECDH` to
the curves `secp256r1`, `secp384r1`, `secp521r1`, `x25519` and `x448`.
