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
staging area or place 1 progress on The Goblins' Assault. Read as its shadow cards resolve,
as Mountain Warg's, once for each such card; the player it attacks chooses. Returned to
the staging area, its attack still resolves.
-}
instance RunMessage WargRider where
  runMessage msg (WargRider attrs) = case msg of
    ResolveShadow eid pid _ | eid == attrs.id -> do
      let faceDown = filter ((`notElem` attrs.enemyShadowRevealed) . (.id)) attrs.shadowCards
      for_ (filter (not . cdShadowEffect . toCardDef) faceDown) \_ ->
        chooseOrRunOneM pid do
          questionLabeled "wargRider.choose"
          causedBy attrs
          when (attrs.placement /= StagingArea) $ labeled "wargRider.return" $ placeEnemy eid StagingArea
          assaultProgressLabeled attrs
      WargRider <$> runMessage msg attrs
    _ -> WargRider <$> runMessage msg attrs
