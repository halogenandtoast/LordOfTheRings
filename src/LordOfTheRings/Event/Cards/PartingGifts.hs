module LordOfTheRings.Event.Cards.PartingGifts (partingGifts) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Leadership))

newtype PartingGifts = PartingGifts EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Move any number of resource tokens from a Leadership hero's resource pool to
-- any other hero's resource pool.
partingGifts :: EventCard PartingGifts
partingGifts = event PartingGifts Cards.partingGifts

instance RunMessage PartingGifts where
  runMessage msg e@(PartingGifts attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      givers <- filter ((> 0) . (.resources)) <$> (traverse getHero =<< select (HeroWithSphere Leadership))
      chooseOneM pid do
        questionLabeled "partingGifts.from"
        causedBy attrs
        targets givers \giver -> do
          others <- select (not_ $ HeroWithId giver.id)
          chooseOneM pid do
            questionLabeled "partingGifts.to"
            causedBy attrs
            targets others \receiver -> chooseOneM pid do
              questionLabeled "chooseX"
              causedBy attrs
              for_ [1 .. giver.resources] \x -> labeledWith "xEquals" [intVar "x" x] do
                removeTokens attrs giver Resource x
                placeResources attrs receiver x
      pure e
    _ -> PartingGifts <$> runMessage msg attrs
