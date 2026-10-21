module LordOfTheRings.Event.Cards.HiddenCache (hiddenCache) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Neutral))

newtype HiddenCache = HiddenCache EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After Hidden Cache is discarded from your deck, add 2 resources to the
-- resource pool of a hero you control.
-- Action: Spend 1 resource to draw 1 card.
hiddenCache :: EventCard HiddenCache
hiddenCache =
  eventWith HiddenCache Cards.hiddenCache \a ->
    a
      & discardWindowL ?~ DiscardedFromDeckWindow After a.owner (Just a.eventCardId)
      & criteriaL .~ exists (HeroControlledBy (PlayerWithId a.owner) <> HeroWithResources)

instance RunMessage HiddenCache where
  runMessage msg e@(HiddenCache attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      spendResources pid Neutral 1
      drawCards pid 1
      pure e
    RespondFromDiscard pid card _ | card.id == toId attrs -> do
      heroes <- select (HeroControlledBy $ PlayerWithId pid)
      chooseOneM pid do
        questionLabeled "hiddenCache.chooseHero"
        causedBy attrs
        targets heroes \h -> placeResources attrs h 2
      pure e
    _ -> HiddenCache <$> runMessage msg attrs
