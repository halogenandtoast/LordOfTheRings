module LordOfTheRings.Location.Cards.CrumbledBattlements (crumbledBattlements) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardType, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (InPlayArea))

newtype CrumbledBattlements = CrumbledBattlements LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

crumbledBattlements :: LocationCard CrumbledBattlements
crumbledBattlements = location CrumbledBattlements Cards.crumbledBattlements

-- | Forced: When you take control of Crumbled Battlements, shuffle the encounter discard
-- pile into the encounter deck and discard cards from the encounter deck until an enemy is
-- discarded. Put the discarded enemy into play engaged with you. With no enemy left in the
-- deck, the discarding stops.
instance RunMessage CrumbledBattlements where
  runMessage msg l@(CrumbledBattlements a) = case msg of
    PlaceLocation lid (InPlayArea pid) | lid == a.id && a.placement /= InPlayArea pid -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      discardTopOfEncounterDeck a 1
      CrumbledBattlements <$> runMessage msg a
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      case (filter isEnemy cards, a.placement) of
        (enemy : _, InPlayArea pid) -> do
          removeFromEncounterDiscard enemy
          putEnemyIntoPlayEngaged pid enemy
        ([], _) -> do
          deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
          when (any isEnemy deck) $ discardTopOfEncounterDeck a 1
        _ -> pure ()
      pure l
    _ -> CrumbledBattlements <$> runMessage msg a
   where
    isEnemy = (== EnemyType) . cdCardType . toCardDef
