//! Define the [`HashFunction`] trait as well as standard hash functions.

/// Abstraction of hash function used by c14n algorithms.
pub trait HashFunction {
    /// Output of the hash function; usually `[u8; N]`.
    type Output: AsRef<[u8]> + Copy + Eq + Ord;

    /// Start the computation of a hash
    fn initialize() -> Self;

    /// Update internal state by hashing `data`
    fn update(&mut self, data: impl AsRef<[u8]>);

    /// Return the hash
    fn finalize(self) -> Self::Output;
}

/// The [SHA-256](https://en.wikipedia.org/wiki/SHA-2) [`HashFunction`]
pub struct Sha256(hmac_sha256::Hash);

impl HashFunction for Sha256 {
    type Output = [u8; 32];

    fn initialize() -> Self {
        Sha256(hmac_sha256::Hash::new())
    }

    fn update(&mut self, data: impl AsRef<[u8]>) {
        self.0.update(data);
    }

    fn finalize(self) -> Self::Output {
        self.0.finalize()
    }
}
