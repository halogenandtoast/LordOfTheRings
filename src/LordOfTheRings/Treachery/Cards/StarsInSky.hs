module LordOfTheRings.Treachery.Cards.StarsInSky (starsInSky) where

import LordOfTheRings.Scenario.Scenarios.TheDruadanForest.Helpers (heroPaysOr)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype StarsInSky = StarsInSky TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

starsInSky :: TreacheryCard StarsInSky
starsInSky = treachery StarsInSky Cards.starsInSky

-- | Prowl 2.
-- When Revealed: Each questing hero must pay 1 resource or it is removed from the quest.
-- Shadow: Discard all resources from the defending character's resource pool.
instance RunMessage StarsInSky where
  runMessage msg t@(StarsInSky attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      heroes <- select CommittedHero
      for_ heroes \h -> heroPaysOr attrs h "starsInSky.removeFromQuest" $ removeFromQuest h
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ [hid | HeroTarget hid <- defenders] \hid -> do
        h <- getHero hid
        when (h.resources > 0) $ removeTokens attrs hid Resource h.resources
      pure t
    _ -> StarsInSky <$> runMessage msg attrs
