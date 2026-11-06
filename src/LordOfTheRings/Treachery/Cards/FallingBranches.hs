module LordOfTheRings.Treachery.Cards.FallingBranches (fallingBranches) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FallingBranches = FallingBranches TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fallingBranches :: TreacheryCard FallingBranches
fallingBranches = treachery FallingBranches Cards.fallingBranches

instance RunMessage FallingBranches where
  runMessage msg t@(FallingBranches a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      eachPlayer \pid -> do
        characters <- selectCharacters (HeroControlledBy (PlayerWithId pid) <> CommittedHero) (AllyControlledBy (PlayerWithId pid) <> CommittedAlly)
        chooseOrRunOneM pid do
          questionLabeled "oldForest.fallingBranches"
          causedBy a
          targets characters \target -> do
            removeFromQuest target
            dealDamage a target $ case target of AllyTarget _ -> 3; _ -> 2
      pure t
    ResolveShadowEffect cid eid _ defenders | cid == a.cardId -> do
      let ally = any (\case AllyTarget _ -> True; _ -> False) defenders
      enemyAttackModifier a eid $ AttackModifier $ if ally then 2 else 1
      pure t
    _ -> FallingBranches <$> runMessage msg a
