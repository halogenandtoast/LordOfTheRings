module LordOfTheRings.Enemy.Cards.TortureMaster (tortureMaster) where

import LordOfTheRings.Card (genEncounterCard)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Trait (Trait (DolGuldur))

newtype TortureMaster = TortureMaster EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

tortureMaster :: EnemyCard TortureMaster
tortureMaster = enemy TortureMaster Cards.tortureMaster

-- | When Revealed: Add 1 resource token to each Dol Guldur location in play.
-- Shadow: Any hero damaged (but not defeated) by this attack is turned face down, as a
-- "prisoner." Then, if the players are on stage 3, return to stage 2B of the quest.
instance RunMessage TortureMaster where
  runMessage msg e@(TortureMaster attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      locations <- select (LocationWithTrait DolGuldur)
      for_ locations \l -> placeResources attrs l 1
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      attackModifier attrs eid AttackDamageTakesPrisoner
      stage <- getMainStage
      when (stage == 3) do
        stage2 <- genEncounterCard Quests.throughTheCaverns
        stage3 <- genEncounterCard Quests.outOfTheDungeons
        afterAttack eid [ResetToStage stage2 [stage3]]
      pure e
    _ -> TortureMaster <$> runMessage msg attrs
