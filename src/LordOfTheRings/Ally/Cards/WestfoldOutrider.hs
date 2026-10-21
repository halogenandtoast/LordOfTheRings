module LordOfTheRings.Ally.Cards.WestfoldOutrider (westfoldOutrider) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype WestfoldOutrider = WestfoldOutrider AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

westfoldOutrider :: AllyCard WestfoldOutrider
westfoldOutrider = ally WestfoldOutrider Cards.westfoldOutrider

-- | Action: Discard Westfold Outrider to choose an enemy not engaged with you. Engage the
-- chosen enemy.
instance HasAbilities WestfoldOutrider where
  getAbilities (WestfoldOutrider a) =
    [ restricted a 1 (exists $ not_ $ EnemyEngagedWith $ PlayerWithId pid) $ ActionAbility Free
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage WestfoldOutrider where
  runMessage msg s@(WestfoldOutrider a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select $ not_ $ EnemyEngagedWith $ PlayerWithId pid
      discard a
      chooseOrRunOneM pid do
        questionLabeled "westfoldOutrider.engage"
        causedBy a
        targets enemies $ engageEnemy pid
      pure s
    _ -> WestfoldOutrider <$> runMessage msg a
