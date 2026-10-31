module LordOfTheRings.Objective.Cards.ThaurdirsSpite (thaurdirsSpite) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Cards.ThaurdirsLegacy (moveToHeroWithMost, thaurdirsBurden)
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))

newtype ThaurdirsSpite = ThaurdirsSpite ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | A burden of the Angmar Awakened campaign, from Deadmen's Dike. Setup: Attach to
Thaurdir (even if he is out of play), which the scenario does as he enters play.
Forced: After a Sorcery treachery is revealed, move Thaurdir's Spite to the hero with the
most damage. (Counts as a Sorcery attachment with the text: "Attached hero gets -2 defense
and cannot be healed.")
-}
thaurdirsSpite :: ObjectiveCard ThaurdirsSpite
thaurdirsSpite = objective ThaurdirsSpite Cards.thaurdirsSpite

instance HasModifiersFor ThaurdirsSpite where
  getModifiersFor (ThaurdirsSpite a) = case a.placement of
    AttachedTo host@(HeroTarget _) -> modified a host [DefenseModifier (-2), CannotBeHealed]
    _ -> pure ()

instance HasAbilities ThaurdirsSpite where
  getAbilities (ThaurdirsSpite a) = thaurdirsBurden a

instance RunMessage ThaurdirsSpite where
  runMessage msg s@(ThaurdirsSpite a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      moveToHeroWithMost a (pure . (.damage))
      pure s
    _ -> ThaurdirsSpite <$> runMessage msg a
