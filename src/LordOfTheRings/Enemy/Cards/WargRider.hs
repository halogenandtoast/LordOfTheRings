module LordOfTheRings.Enemy.Cards.WargRider (wargRider) where

import LordOfTheRings.Card (cdShadowEffect, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheFateOfWilderland.Helpers (assaultProgressLabeled)

newtype WargRider = WargRider EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wargRider :: EnemyCard WargRider
wargRider = enemy WargRider Cards.wargRider

{- | Forced: After Warg-rider is dealt a shadow card with no effect, either return it to the
staging area or place 1 progress on The Goblins' Assault. Read as each shadow card turns
faceup ('revealedShadowCards'), once for each such card; the player it attacks chooses. Returned to
the staging area, its attack still resolves.
-}
instance RunMessage WargRider where
  runMessage msg (WargRider attrs) = case msg of
    CheckWindows ws -> do
      for_ [pid | (c, pid, _) <- revealedShadowCards attrs ws, not (cdShadowEffect $ toCardDef c)] \pid ->
        chooseOrRunOneM pid do
          questionLabeled "wargRider.choose"
          causedBy attrs
          when (attrs.placement /= StagingArea) $ labeled "wargRider.return" $ placeEnemy attrs.id StagingArea
          assaultProgressLabeled attrs
      WargRider <$> runMessage msg attrs
    _ -> WargRider <$> runMessage msg attrs
