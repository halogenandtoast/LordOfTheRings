module LordOfTheRings.Ally.Cards.DunedainHunter (dunedainHunter) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (Card (EncounterCard), CardType (EnemyType), cdUnique, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)

newtype DunedainHunter = DunedainHunter AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunedainHunter :: AllyCard DunedainHunter
dunedainHunter = ally DunedainHunter Cards.dunedainHunter

-- | Forced: After Dúnedain Hunter enters play, search the top 5 cards of the encounter deck
-- for a non-unique enemy and put it into play engaged with you. If no enemy enters play by
-- this effect, discard Dúnedain Hunter. Shuffle the encounter deck.
instance HasAbilities DunedainHunter where
  getAbilities (DunedainHunter a) = [mkAbility a 1 $ forced $ entersPlay a]

instance RunMessage DunedainHunter where
  runMessage msg s@(DunedainHunter a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      top <- take 5 . (.encounterDeck) . toAttrs <$> getsGame (.scenario)
      let enemies = filter (\c -> cardMatches (CardWithType EnemyType) c && not (cdUnique $ toCardDef c)) top
      if null enemies
        then discard a a
        else chooseOneM pid do
          questionLabeled "dunedainHunter.search"
          causedBy a
          showing (map EncounterCard top)
          targets enemies \c -> do
            removeFromEncounterDeck c
            putEnemyIntoPlayEngaged pid c
      shuffleEncounterDeck
      pure s
    _ -> DunedainHunter <$> runMessage msg a
