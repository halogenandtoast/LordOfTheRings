module LordOfTheRings.Location.Cards.JaggedCavern (jaggedCavern) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheDrownedRuins.Helpers (mayFlipToGrottoSide)

-- | The Underwater side of Overgrown Passage.
newtype JaggedCavern = JaggedCavern LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

jaggedCavern :: LocationCard JaggedCavern
jaggedCavern = location JaggedCavern Cards.jaggedCavern

{- | While Jagged Cavern is the active location, the first treachery card revealed each
round gains doomed 1 and surge.
ponytail: the first one revealed while it is active, once it has been revealed
Forced: If Jagged Cavern is the active location at the end of the quest phase, each player
must discard each card in his or her hand. Then, you may flip Jagged Cavern to its Grotto
side.
-}
instance HasAbilities JaggedCavern where
  getAbilities (JaggedCavern a) =
    [ ability
    | a.placement == ActiveLocation
    , ability <-
        [ limited (GroupLimit PerRound 1) $ mkAbility a 1 $ forced $ TreacheryRevealedWindow After
        , mkAbility a 2 $ forced $ PhaseEndsWindow When QuestPhase
        ]
    ]

instance RunMessage JaggedCavern where
  runMessage msg l@(JaggedCavern a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      raiseThreatFromDoomed 1
      revealEncounterCard
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      eachPlayer discardHand
      mayFlipToGrottoSide a
      pure l
    _ -> JaggedCavern <$> runMessage msg a
