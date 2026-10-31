module LordOfTheRings.Location.Cards.SubmergedCrawlway (submergedCrawlway) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase, QuestPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheDrownedRuins.Helpers (mayFlipToGrottoSide)

-- | The Underwater side of Sharp Precipice.
newtype SubmergedCrawlway = SubmergedCrawlway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

submergedCrawlway :: LocationCard SubmergedCrawlway
submergedCrawlway = location SubmergedCrawlway Cards.submergedCrawlway

{- | While Submerged Crawlway is the active location, deal each engaged enemy 1 additional
shadow card at the beginning of the combat phase.
Forced: If Submerged Crawlway is the active location at the end of the quest phase, each
player must search the encounter deck and discard pile for an enemy and add it to the
staging area. Then, you may flip Submerged Crawlway to its Grotto side.
-}
instance HasAbilities SubmergedCrawlway where
  getAbilities (SubmergedCrawlway a) =
    [ ability
    | a.placement == ActiveLocation
    , ability <-
        [ mkAbility a 1 $ forced $ PhaseBeginsWindow After CombatPhase
        , mkAbility a 2 $ forced $ PhaseEndsWindow When QuestPhase
        ]
    ]

instance RunMessage SubmergedCrawlway where
  runMessage msg l@(SubmergedCrawlway a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      engaged <- select (EnemyEngagedWith AnyPlayer)
      for_ engaged dealShadowCard
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      eachPlayer \p -> searchEncounterDeckAndDiscard p a (CardWithType EnemyType)
      mayFlipToGrottoSide a
      pure l
    _ -> SubmergedCrawlway <$> runMessage msg a
