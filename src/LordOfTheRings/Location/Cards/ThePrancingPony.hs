module LordOfTheRings.Location.Cards.ThePrancingPony (thePrancingPony) where

import LordOfTheRings.Card (CardType (AllyType, EnemyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype ThePrancingPony = ThePrancingPony LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

thePrancingPony :: LocationCard ThePrancingPony
thePrancingPony = location ThePrancingPony Cards.thePrancingPony

-- | Response: After the players travel here, the first player puts 1 ally into play from
-- his hand.
-- Forced: After The Prancing Pony leaves play, discard cards from the top of the encounter
-- deck until X enemies have been discarded. X is the number of players in the game. Put
-- each enemy discarded by this effect into play engaged with the first player.
instance HasAbilities ThePrancingPony where
  getAbilities (ThePrancingPony a) =
    [ mkAbility a 1 $ response $ TraveledWindow After a.id
    , mkAbility a 2 $ forced $ LocationLeftPlayWindow After a.id
    ]

-- | The enemies are read off the deck as the discarding starts: The Prancing Pony is out of
-- play by then, so nothing reads them back to it.
instance RunMessage ThePrancingPony where
  runMessage msg l@(ThePrancingPony a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      player <- getPlayer lead
      allies <- filterM passesUnique [c | c <- player.hand, c.cardType == AllyType]
      chooseOrRunOneM lead do
        questionLabeled "chooseAllyToPutIntoPlay"
        causedBy a
        targets allies \c -> putAllyIntoPlayFromHand lead c.id
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      lead <- getLead
      x <- length <$> getRemainingPlayers
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      discardEncounterCardsUntilCount a x (CardWithType EnemyType)
      for_ (take x $ filter (cardMatches $ CardWithType EnemyType) deck) \c -> do
        removeFromEncounterDiscard c
        putEnemyIntoPlayEngaged lead c
      pure l
    _ -> ThePrancingPony <$> runMessage msg a
