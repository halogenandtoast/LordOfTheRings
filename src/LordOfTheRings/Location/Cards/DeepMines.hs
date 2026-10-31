module LordOfTheRings.Location.Cards.DeepMines (deepMines) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Goblin))

newtype DeepMines = DeepMines LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

deepMines :: LocationCard DeepMines
deepMines = location DeepMines Cards.deepMines

-- | Forced: When a Goblin enemy enters play, add 1 resource to that enemy if Deep-mines is
-- in the staging area.
-- ponytail: read as a Goblin entering the staging area, the way encounter enemies enter
-- play; one put into play engaged takes none.
instance HasAbilities DeepMines where
  getAbilities (DeepMines a) =
    [ restricted a 1 (exists $ LocationWithId a.id <> LocationInStagingArea)
        $ forced
        $ EnemyWithTraitEntersStagingAreaWindow After Goblin
    ]

-- | Shadow: Attacking enemy gets +1 attack. Then, if the attacking enemy is a Goblin, deal
-- it another shadow card.
instance RunMessage DeepMines where
  runMessage msg l@(DeepMines a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [eid | Window _ (EntersStagingArea (EnemyTarget eid)) <- ws] \eid -> placeResources a eid 1
      pure l
    ResolveShadowEffect cid eid pid defenders | cid == a.cardId -> do
      enemyGetsAttack a eid 1
      whenM (selectAny $ EnemyWithId eid <> EnemyWithTrait Goblin) $ dealShadowCardsDuringAttack eid pid defenders 1
      pure l
    _ -> DeepMines <$> runMessage msg a
