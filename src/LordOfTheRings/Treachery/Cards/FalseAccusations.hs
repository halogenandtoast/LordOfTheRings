module LordOfTheRings.Treachery.Cards.FalseAccusations (falseAccusations) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FalseAccusations = FalseAccusations TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

falseAccusations :: TreacheryCard FalseAccusations
falseAccusations = treachery FalseAccusations Cards.falseAccusations

-- | When Revealed: Each player must either raise his threat by 5 or exhaust each ready
-- character he controls.
-- Shadow: After this attack, return attacking enemy to the staging area. Then, it makes
-- engagement checks against each player.
instance RunMessage FalseAccusations where
  runMessage msg t@(FalseAccusations attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        heroes <- select (HeroControlledBy (PlayerWithId pid) <> ReadyHero)
        allies <- select (AllyControlledBy (PlayerWithId pid) <> ReadyAlly)
        chooseOneM pid do
          questionLabeled "falseAccusations.choose"
          causedBy attrs
          labeled "falseAccusations.threat" $ encounterRaisesThreat pid 5
          labeled "falseAccusations.exhaust" do
            traverse_ exhaust heroes
            traverse_ exhaust allies
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      returnToStagingAfterAttack eid
      afterAttack eid [DoStep 1 msg]
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      makeEngagementChecks
      pure t
    _ -> FalseAccusations <$> runMessage msg attrs
