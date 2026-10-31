module LordOfTheRings.Location.Cards.NestOfHorrors (nestOfHorrors) where

import LordOfTheRings.Card (CardDef (..), CardType (EnemyType), toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Nameless))

newtype NestOfHorrors = NestOfHorrors LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

nestOfHorrors :: LocationCard NestOfHorrors
nestOfHorrors = location NestOfHorrors Cards.nestOfHorrors

-- | While Nest of Horrors is in the staging area, it gains: "Forced: When one or more cards
-- are attached to a Nameless enemy from the top of a player's deck, attach an additional
-- card from the top of that player's deck." (See
-- 'LordOfTheRings.Scenario.Scenarios.FoundationsOfStone.Helpers.attachFromTopOfDeck'.)
-- Travel: Each player at this stage discards the top card of the encounter deck to travel
-- here. Reveal each Nameless enemy discarded by this effect.
instance RunMessage NestOfHorrors where
  runMessage msg (NestOfHorrors a) = case msg of
    TravelTo lid | lid == a.id -> do
      players <- length <$> getRemainingPlayers
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let discarded = take players deck
          nameless c = cdCardType (toCardDef c) == EnemyType && Nameless `member` cdCardTraits (toCardDef c)
      discardTopOfEncounterDeck a (length discarded)
      traverse_ revealFromEncounterDiscard (filter nameless discarded)
      NestOfHorrors <$> runMessage msg a
    _ -> NestOfHorrors <$> runMessage msg a
