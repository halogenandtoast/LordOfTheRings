module LordOfTheRings.Treachery.Cards.BrokenRudder (brokenRudder) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

{- | Counts as a Condition attachment with the text: "During sailing tests, for each
success symbol found, cancel it unless you deal 1 damage to attached objective." A Storm
on Cobas Haven asks it as the test's result comes, and its *Forced as a Sailing test
discards it.
-}
newtype BrokenRudder = BrokenRudder TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

brokenRudder :: TreacheryCard BrokenRudder
brokenRudder = treachery BrokenRudder Cards.brokenRudder

-- | Action: Exhaust 3 characters you control to discard Broken Rudder.
instance HasAbilities BrokenRudder where
  getAbilities (BrokenRudder a) = case a.placement of
    AttachedTo _ ->
      [ anyPlayer
          $ restricted a 1 (CharactersAtLeast 3 (HeroControlledBy ActivePlayer <> ReadyHero) (AllyControlledBy ActivePlayer <> ReadyAlly))
          $ ActionAbility
          $ Costs (replicate 3 $ ExhaustCharacterCost (HeroControlledBy ActivePlayer) (AllyControlledBy ActivePlayer))
      ]
    _ -> []

-- | When Revealed: Attach to a Ship objective.
instance RunMessage BrokenRudder where
  runMessage msg t@(BrokenRudder attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      ships <- select shipObjective
      chooseOrRunOneM lead do
        questionLabeled "brokenRudder.attach"
        causedBy attrs
        targets ships \s -> attachTreachery attrs.treacheryId s
      pure t
    -- the characters are exhausted as the cost is paid
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discard attrs attrs
      pure t
    _ -> BrokenRudder <$> runMessage msg attrs
