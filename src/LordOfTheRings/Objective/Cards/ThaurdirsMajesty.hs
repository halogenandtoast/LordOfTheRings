module LordOfTheRings.Objective.Cards.ThaurdirsMajesty (thaurdirsMajesty) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Cards.ThaurdirsLegacy (moveToHeroWithMost, thaurdirsBurden)
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))

newtype ThaurdirsMajesty = ThaurdirsMajesty ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | A burden of the Angmar Awakened campaign, from Deadmen's Dike. Setup: Attach to
Thaurdir (even if he is out of play), which the scenario does as he enters play.
Forced: After a Sorcery treachery is revealed, move Thaurdir's Majesty to the hero with the
most non-objective attachments. (Counts as a Sorcery attachment with the text: "Attached
hero gets -2 attack and cannot commit to the quest.")
-}
thaurdirsMajesty :: ObjectiveCard ThaurdirsMajesty
thaurdirsMajesty = objective ThaurdirsMajesty Cards.thaurdirsMajesty

instance HasModifiersFor ThaurdirsMajesty where
  getModifiersFor (ThaurdirsMajesty a) = case a.placement of
    AttachedTo host@(HeroTarget _) -> modified a host [AttackModifier (-2), CannotCommitToQuest]
    _ -> pure ()

instance HasAbilities ThaurdirsMajesty where
  getAbilities (ThaurdirsMajesty a) = thaurdirsBurden a

instance RunMessage ThaurdirsMajesty where
  runMessage msg s@(ThaurdirsMajesty a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      attachments <- getsGame (toList . (.entities.attachments))
      moveToHeroWithMost a \h -> pure $ length [() | att <- attachments, att.placement == AttachedTo (toTarget h.id)]
      pure s
    _ -> ThaurdirsMajesty <$> runMessage msg a
