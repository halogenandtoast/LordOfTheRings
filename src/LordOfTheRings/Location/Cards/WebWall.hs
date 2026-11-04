module LordOfTheRings.Location.Cards.WebWall (webWall) where

import LordOfTheRings.Card (CardDef (..), CardType (EnemyType), toCardDef)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (shadowGuardTargets)
import LordOfTheRings.Trait (Trait (Spider))

newtype WebWall = WebWall LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

webWall :: LocationCard WebWall
webWall = location WebWall Cards.webWall

-- | When Revealed: Reveal the topmost Spider enemy in the encounter discard pile and add it
-- to the staging area.
-- Shadow: The player guarding Gollum must discard all resources from each of his heroes'
-- resource pools.
instance RunMessage WebWall where
  runMessage msg l@(WebWall a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      let spider c = cdCardType (toCardDef c) == EnemyType && Spider `member` cdCardTraits (toCardDef c)
      traverse_ revealFromEncounterDiscard . find spider =<< getEncounterDiscard
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      shadowGuardTargets eid >>= traverse_ \guard' -> do
        heroes <- select (HeroControlledBy $ PlayerWithId guard')
        for_ heroes \h -> do
          n <- (.resources) <$> getHero h
          when (n > 0) $ removeTokens a h Resource n
      pure l
    _ -> WebWall <$> runMessage msg a
