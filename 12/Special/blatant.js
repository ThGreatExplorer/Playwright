class Blatant {
    violation() {
        return this; // Returns Blatant instance, not a number
    }
}

module.exports = { Blatant };