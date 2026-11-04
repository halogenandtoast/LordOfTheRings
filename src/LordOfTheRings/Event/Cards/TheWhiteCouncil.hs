module LordOfTheRings.Event.Cards.TheWhiteCouncil (theWhiteCouncil) where

import Data.Bits (setBit, testBit)
import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (eventsMayShuffleDiscardIntoDeck)

newtype TheWhiteCouncil = TheWhiteCouncil EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, Entity)

theWhiteCouncil :: EventCard TheWhiteCouncil
theWhiteCouncil = event TheWhiteCouncil Cards.theWhiteCouncil

-- | X is the number of players in the game: its cost of X, paid as 0, is raised by as much.
instance HasModifiersFor TheWhiteCouncil where
  getModifiersFor (TheWhiteCouncil a) = do
    players <- length <$> getRemainingPlayers
    modifySelf a [CostModifier players]

-- | Action: Starting with the first player, each player chooses 1 different option: ready a
-- hero he controls, add 1 resource to the resource pool of a hero he controls, draw 1
-- card, or shuffle 1 card from his discard pile into his deck. That option is not there
-- while event cards cannot shuffle from a discard pile, as Passage into Mordor reads.
--
-- Each step is the player's seat times 16, with a bit for each option already chosen.
instance RunMessage TheWhiteCouncil where
  runMessage msg e@(TheWhiteCouncil attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      doStep 0 msg
      pure e
    DoStep step inner@(PlayEvent _ card _) | card.id == toId attrs -> do
      let (seat, chosen) = step `divMod` 16
          open bit = not (testBit chosen bit)
          next bit = doStep ((seat + 1) * 16 + setBit chosen bit) inner
      players <- getRemainingPlayers
      shuffles <- eventsMayShuffleDiscardIntoDeck
      for_ (headMay $ drop seat players) \p -> do
        heroes <- select (HeroControlledBy $ PlayerWithId p)
        discardPile <- (.discard) <$> getPlayer p
        chooseOneM p do
          questionLabeled "theWhiteCouncil.choose"
          causedBy attrs
          when (open 0 && notNull heroes) $ labeled "theWhiteCouncil.ready" do
            chooseOrRunOneM p do
              questionLabeled "chooseHeroToReady"
              causedBy attrs
              targets heroes ready
            next 0
          when (open 1 && notNull heroes) $ labeled "theWhiteCouncil.resource" do
            chooseOrRunOneM p do
              questionLabeled "chooseHeroForResource"
              causedBy attrs
              targets heroes \h -> placeResources attrs h 1
            next 1
          when (open 2) $ labeled "theWhiteCouncil.draw" do
            drawCards attrs p 1
            next 2
          when (open 3 && shuffles && notNull discardPile) $ labeled "theWhiteCouncil.shuffle" do
            chooseOrRunOneM p do
              questionLabeled "theWhiteCouncil.chooseCard"
              causedBy attrs
              showing discardPile
              targets [pc | PlayerCard pc <- discardPile] \pc -> shuffleFromDiscardIntoDeck p pc.id
            next 3
      pure e
    _ -> TheWhiteCouncil <$> runMessage msg attrs
