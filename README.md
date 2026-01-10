Bitcoin Prediction Market on Stacks

This project is a decentralized prediction market smart contract written in Clarity for the Stacks blockchain. It allows users to create predictions about Bitcoin-related events, place bets, and claim winnings based on outcomes.

## Features

- **Create Prediction:** Any user can create a prediction by staking STX and specifying a condition and deadline.
- **Place Bet:** Users can bet on the outcome (yes/no) of any active prediction before its deadline.
- **Reveal Outcome:** The creator of a prediction can reveal the outcome after the deadline.
- **Claim Winnings:** Winning bettors can claim their share of the pool after the outcome is revealed.
- **Refund Creator:** If no bets are placed, the creator can refund their stake.
- **Contract Fee:** A small fee is deducted from winnings and sent to the contract.

## Smart Contract Details

- **Minimum Stake:** 1 STX required to create a prediction.
- **Minimum Bet:** 0.1 STX required to place a bet.
- **Fee:** 5% of winnings (configurable).
- **Data Maps:** Tracks predictions and bets.
- **Error Handling:** Returns clear error codes for invalid actions.

## Usage

Deploy the `PredictionPool.clar` contract to the Stacks blockchain. Interact with the contract using Clarity calls:

- `create-prediction`
- `place-bet`
- `reveal-outcome`
- `claim-winnings`
- `refund-creator`
- Read-only functions for querying predictions, bets, and contract fee.

## Requirements

- Stacks blockchain
- Clarity smart contract language

## File Structure

- `contracts/PredictionPool.clar` — Main smart contract

## Disclaimer

This contract does not include a frontend or off-chain oracle integration. All logic is on-chain and relies on the creator to reveal outcomes.

---
