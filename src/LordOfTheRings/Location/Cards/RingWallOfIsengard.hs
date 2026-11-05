module LordOfTheRings.Location.Cards.RingWallOfIsengard (ringWallOfIsengard) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheRoadToIsengard.Helpers (wizardry)

newtype RingWallOfIsengard = RingWallOfIsengard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ringWallOfIsengard :: LocationCard RingWallOfIsengard
ringWallOfIsengard = location RingWallOfIsengard Cards.ringWallOfIsengard

{- | While Ring-wall of Isengard is in the staging area, it gains: "Forced: After any amount
of damage is healed from a character, its owner discards 1 card from his hand."
Wizardry: Each player discards 1 random card from his hand (see 'wizardry').
-}
instance HasAbilities RingWallOfIsengard where
  getAbilities (RingWallOfIsengard a) =
    [mkAbility a 1 $ forced $ HealedByOtherWindow After (toSource a) | a.placement == StagingArea]
      <> [mkAbility a 2 $ forced $ ExploredWindow When a.id]

instance RunMessage RingWallOfIsengard where
  runMessage msg l@(RingWallOfIsengard a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [t | Window _ (Healed t _) <- ws] \case
        HeroTarget hid -> (\h -> chooseAndDiscardFromHand h.owner a 1) =<< getHero hid
        AllyTarget aid -> (\x -> chooseAndDiscardFromHand x.owner a 1) =<< getAlly aid
        _ -> pure ()
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      wizardry a (toCardCode a) =<< getRemainingPlayers
      pure l
    _ -> RingWallOfIsengard <$> runMessage msg a
