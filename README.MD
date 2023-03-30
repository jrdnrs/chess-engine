## The little (chess) engine that could

- represents board using u64 (bitboards)
- magic bitboards are used for rook and bishop attacks
- legal-only move generator
- negamax search with alpha-beta pruning
- move ordering to improve pruning
- quiescence search