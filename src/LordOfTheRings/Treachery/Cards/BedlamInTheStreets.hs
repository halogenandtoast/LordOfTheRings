module LordOfTheRings.Treachery.Cards.BedlamInTheStreets (bedlamInTheStreets) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BedlamInTheStreets = BedlamInTheStreets TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bedlamInTheStreets :: TreacheryCard BedlamInTheStreets
bedlamInTheStreets = treachery BedlamInTheStreets Cards.bedlamInTheStreets

-- | When Revealed: Each player must either remove a hero he controls from the quest or
-- engage an enemy in the staging area. Each enemy engaged by this effect makes an
-- immediate attack.
-- Shadow: Attacking enemy gets +2 attack.
instance RunMessage BedlamInTheStreets where
  runMessage msg t@(BedlamInTheStreets attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        questing <- select (HeroControlledBy (PlayerWithId pid) <> CommittedHero)
        enemies <- select EnemyInStagingArea
        chooseOneM pid do
          questionLabeled "bedlamInTheStreets.choose"
          causedBy attrs
          targets questing removeFromQuest
          targets enemies \e -> do
            engageEnemy pid e
            immediateAttack pid e
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 2
      pure t
    _ -> BedlamInTheStreets <$> runMessage msg attrs
