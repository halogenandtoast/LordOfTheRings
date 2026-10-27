module LordOfTheRings.Location.Cards.DarkPit (darkPit) where

import LordOfTheRings.Card (CardCost (StaticCost), cdCost, toCardDef)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.ShadowAndFlame.Helpers

newtype DarkPit = DarkPit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkPit :: LocationCard DarkPit
darkPit = location DarkPit Cards.darkPit

-- | While Dark Pit is the active location it gains: "Refresh Action: Exhaust X characters
-- (maximum 3) you control to discard the top X cards of your deck. If all discarded cards
-- have a higher combined printed cost than the remaining hit points of Durin's Bane,
-- discard Durin's Bane from play."
instance HasAbilities DarkPit where
  getAbilities (DarkPit a) =
    [ anyPlayer
        $ restricted a 1 (DuringPhase RefreshPhase <> CharactersAtLeast 1 (HeroControlledBy ActivePlayer <> ReadyHero) (AllyControlledBy ActivePlayer <> ReadyAlly))
        $ ActionAbility Free
    | a.placement == ActiveLocation
    ]

-- | Discarding Durin's Bane this way wins the game, as Last Lord of Moria reads; Dark Pit
-- enters play only at that stage.
instance RunMessage DarkPit where
  runMessage msg l@(DarkPit attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      doStep 0 msg
      pure l
    -- one character at a time, until 3 are exhausted or the player stops
    DoStep x use@(UseCardAbility pid (isSource attrs -> True) 1 _) -> do
      able <- selectCharacters (HeroControlledBy (PlayerWithId pid) <> ReadyHero) (AllyControlledBy (PlayerWithId pid) <> ReadyAlly)
      if x >= 3 || null able
        then discardFor pid x
        else chooseOneM pid do
          questionLabeledWith "darkPit.exhaust" [intVar "count" x]
          causedBy attrs
          targets able \c -> do
            exhaust c
            doStep (x + 1) use
          when (x > 0) $ labeled "darkPit.done" $ discardFor pid x
      pure l
    _ -> DarkPit <$> runMessage msg attrs
   where
    discardFor pid x = do
      deck <- (.deck) <$> getPlayer pid
      -- a card with no printed number (X) costs 0
      let printed c = case cdCost (toCardDef c) of
            Just (StaticCost n) -> n
            _ -> 0
          total = sum $ map printed $ take x deck
      discardTopOfDeck pid x
      bane <- getDurinsBane
      for_ bane \eid -> do
        left <- durinsBaneHitPointsLeft eid
        when (total > left) do
          discard eid
          winGame
