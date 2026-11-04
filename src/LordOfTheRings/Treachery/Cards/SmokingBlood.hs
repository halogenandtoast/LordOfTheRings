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
      -- undefended, the hero is the one the attack's damage goes to, known once it is dealt
      if null defenders
        then afterAttack eid [DoStep 1 msg]
        else moveOnto eid pid defenders
      pure t
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == attrs.cardId -> do
      damaged <- attackDamaged eid
      heroes <- filterM (\h -> selectAny (HeroWithId h)) (ordNub [h | (HeroTarget h, n) <- damaged, n > 0])
      moveOnto eid pid (map toTarget heroes)
      pure t
    _ -> SmokingBlood <$> runMessage msg attrs
   where
    -- moved, not dealt: nothing answers it as damage dealt
    moveOnto eid pid characters = do
      present <- selectAny (EnemyWithId eid)
      damage <- if present then (.damage) <$> getEnemy eid else pure 0
      when (damage > 0) $ chooseOrRunOneM pid do
        questionLabeled "smokingBlood.moveDamage"
        causedBy attrs
        targets characters \c -> moveDamage attrs eid c damage
