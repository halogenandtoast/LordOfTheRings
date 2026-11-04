module LordOfTheRings.Location.Cards.SlopesOfCirithGorgor (slopesOfCirithGorgor) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype SlopesOfCirithGorgor = SlopesOfCirithGorgor LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

slopesOfCirithGorgor :: LocationCard SlopesOfCirithGorgor
slopesOfCirithGorgor = location SlopesOfCirithGorgor Cards.slopesOfCirithGorgor

-- | Forced: When Slopes of Cirith Gorgor leaves play, the first player shuffles the
-- encounter discard pile into the encounter deck and discards cards from the top until an
-- enemy is discarded. Then, he puts that enemy into play engaged with him. Read in the
-- window as it leaves, the location answering from out of play.
instance HasAbilities SlopesOfCirithGorgor where
  getAbilities (SlopesOfCirithGorgor a) = [mkAbility a 1 $ forced $ LocationLeftPlayWindow After a.id]

instance RunMessage SlopesOfCirithGorgor where
  runMessage msg l@(SlopesOfCirithGorgor a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure l
    -- the deck is read once it is shuffled
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      discardEncounterCardsUntil a (CardWithType EnemyType)
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      lead <- getLead
      for_ (find (cardMatches $ CardWithType EnemyType) cards) \card -> do
        removeFromEncounterDiscard card
        putEnemyIntoPlayEngaged lead card
      pure l
    _ -> SlopesOfCirithGorgor <$> runMessage msg a
