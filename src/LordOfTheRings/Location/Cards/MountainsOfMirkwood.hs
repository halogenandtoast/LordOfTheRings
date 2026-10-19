module LordOfTheRings.Location.Cards.MountainsOfMirkwood (mountainsOfMirkwood) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype MountainsOfMirkwood = MountainsOfMirkwood LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mountainsOfMirkwood :: LocationCard MountainsOfMirkwood
mountainsOfMirkwood = location MountainsOfMirkwood Cards.mountainsOfMirkwood

-- | Response: After Mountains of Mirkwood leaves play as an explored location, each player
-- may search the top 5 cards of his deck for 1 card and add it to his hand. Shuffle the
-- rest of the searched cards back into their owners' decks.
instance HasAbilities MountainsOfMirkwood where
  getAbilities (MountainsOfMirkwood a) = [mkAbility a 1 $ response $ ExploredWindow When a.id]

-- | Travel: Reveal the top card of the encounter deck and add it to the staging area to
-- travel here.
instance RunMessage MountainsOfMirkwood where
  runMessage msg l@(MountainsOfMirkwood a) = case msg of
    TravelTo lid | lid == a.id -> do
      revealEncounterCard
      pure l
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> do
        top <- take 5 . (.deck) <$> getPlayer p
        push
          $ Ask p
          $ QuestionLabel (ikey "label.mountainsOfMirkwood.search") (Just $ toTarget a) []
          $ SearchCards top
          $ ChooseOne
          $ [targetLabel (CardIdTarget c.id) [TakeFromDeck p c.id, ShufflePlayerDeck p] | c <- top]
          <> [Label (ikey "label.takeNothing") [ShufflePlayerDeck p]]
      pure l
    _ -> MountainsOfMirkwood <$> runMessage msg a
