module LordOfTheRings.Quest.Cards.HuntingTheBeast (huntingTheBeast) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWitheredHeath.Helpers (dragonSignsInVictoryDisplay, healSetAsideColdDrake, setAsideColdDrake)

newtype HuntingTheBeast = HuntingTheBeast QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

huntingTheBeast :: QuestCard HuntingTheBeast
huntingTheBeast = quest HuntingTheBeast Cards.huntingTheBeast

-- | The players cannot defeat this stage unless at least 3 Dragon Signs are in the victory
-- display.
instance HasModifiersFor HuntingTheBeast where
  getModifiersFor (HuntingTheBeast a) = when a.flipped do
    signs <- dragonSignsInVictoryDisplay
    when (signs < 3) $ modifySelf a [CannotBeDefeated]

-- | Forced: After the active location is explored, look at the top 3 cards of the
-- encounter deck. Reveal 1 of those cards and discard the rest.
-- Forced: At the end of the round, remove 3 damage tokens from the set-aside Cold-Drake.
instance HasAbilities HuntingTheBeast where
  getAbilities (HuntingTheBeast a) =
    guard a.flipped
      *> [ mkAbility a 1 $ forced $ ActiveLocationExploredWindow After
         , mkAbility a 2 $ forced RoundEndsWindow
         ]

{- | When Revealed: Shuffle 2 set-aside Dragon Sign objectives into the Caves deck. Set
Cold-Drake aside, out of play. (Do not remove damage tokens from it.) Shuffle the encounter
discard pile into the encounter deck and discard cards from the top until X locations are
discarded, where X is the number of players. Add each discarded location to the staging
area. The shuffled deck is read back in a later step.
-}
instance RunMessage HuntingTheBeast where
  runMessage msg q@(HuntingTheBeast attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      HuntingTheBeast <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      signs <- getSetAside (cardIs Objectives.dragonSign)
      for_ (take 2 signs) shuffleIntoCavesDeck
      setAsideColdDrake
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount attrs players locationCard
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (filter (cardMatches locationCard) cards) addToStagingFromDiscard
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      revealOneOfTopEncounterCards lead attrs 3
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      healSetAsideColdDrake 3
      pure q
    _ -> HuntingTheBeast <$> runMessage msg attrs
   where
    locationCard = CardWithType LocationType
