module LordOfTheRings.Treachery.Cards.SmokingBlood (smokingBlood) where

import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SmokingBlood = SmokingBlood TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

smokingBlood :: TreacheryCard SmokingBlood
smokingBlood = treachery SmokingBlood Cards.smokingBlood

-- | When Revealed: Remove all damage from all enemies. Then, each player raises his threat
-- by the amount of damage just removed from his engaged enemies. If no damage is removed by
-- this effect, Smoking Blood gains surge.
-- Shadow: Move all damage on this enemy to the defending character. (If undefended, move
-- the damage to the hero damaged by this attack.)
instance RunMessage SmokingBlood where
  runMessage msg t@(SmokingBlood attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      enemies <- filter ((> 0) . (.damage)) <$> (traverse getEnemy =<< select AnyEnemy)
      for_ enemies \e -> healAllDamage attrs e
      players <- getRemainingPlayers
      for_ players \p -> do
        let removed = sum [e.damage | e <- enemies, e.placement == EngagedWith p]
        when (removed > 0) $ encounterRaisesThreat p removed
      when (null enemies) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      damage <- (.damage) <$> getEnemy eid
      when (damage > 0) do
        -- ponytail: undefended, the player picks the hero; the damage is not yet dealt
        targets' <- if null defenders then map toTarget <$> select (HeroControlledBy $ PlayerWithId pid) else pure defenders
        chooseOrRunOneM pid do
          questionLabeled "smokingBlood.moveDamage"
          causedBy attrs
          targets targets' \c -> do
            healDamage attrs eid damage
            dealDamage (EnemySource eid) c damage
      pure t
    _ -> SmokingBlood <$> runMessage msg attrs
