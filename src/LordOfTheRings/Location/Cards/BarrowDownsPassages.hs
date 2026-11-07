module LordOfTheRings.Location.Cards.BarrowDownsPassages (greatBarrow, standingStones) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameRevealer)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Quest.CardDefs qualified as Quests

newtype GreatBarrow = GreatBarrow LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

greatBarrow :: LocationCard GreatBarrow
greatBarrow = location GreatBarrow Cards.greatBarrow

instance HasModifiersFor GreatBarrow where
  getModifiersFor (GreatBarrow a) = modifySelf a [ImmuneToPlayerCardEffects, CannotBeTraveledTo]

instance HasAbilities GreatBarrow where
  getAbilities (GreatBarrow a) = [mkAbility a 1 $ forced $ CardEntersStagingAreaWindow After (toTarget a)]

instance RunMessage GreatBarrow where
  runMessage msg l@(GreatBarrow a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      you <- getRevealer
      characters <- selectCharacters (HeroControlledBy $ PlayerWithId you) (AllyControlledBy $ PlayerWithId you)
      traverse_ removeFromQuest characters
      stages <- getSetAside (cardIs Quests.trappedInsideABarrow)
      let stage = fromJustNote "Great Barrow: no unused stage 4" $ headMay stages
      removeSetAsideFromGame [stage]
      separatePlayer you
      push $ AdvanceToQuest stage
      pure l
    _ -> GreatBarrow <$> runMessage msg a

newtype StandingStones = StandingStones LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

standingStones :: LocationCard StandingStones
standingStones = location StandingStones Cards.standingStones

instance HasModifiersFor StandingStones where
  getModifiersFor (StandingStones a) = do
    modifySelf a [ImmuneToPlayerCardEffects]
    available <- getsGame $ \g ->
      let s = toAttrs g.scenario
       in any ((== toCardCode Cards.greatBarrow) . toCardCode) s.encounterDeck
          || any ((== toCardCode Cards.greatBarrow) . toCardCode) s.encounterDiscard
          || any ((== toCardCode Cards.greatBarrow) . toCardCode) s.victoryDisplay
    unless available $ modifySelf a [CannotBeTraveledTo]

instance RunMessage StandingStones where
  runMessage msg l@(StandingStones a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      old <- getsGame gameRevealer
      push $ SetRevealer (Just lead)
      searchEncounterCardsAndVictoryDisplayToStaging lead a (cardIs Cards.greatBarrow)
      push $ SetRevealer old
      pure l
    _ -> StandingStones <$> runMessage msg a
