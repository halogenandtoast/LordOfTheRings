module LordOfTheRings.Attachment.Cards.GreyCloak (greyCloak) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Sphere (Sphere (Spirit))
import LordOfTheRings.Trait (Trait (Scout))

newtype GreyCloak = GreyCloak AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a spirit or Scout character. Limit 1 per character.
greyCloak :: AttachmentCard GreyCloak
greyCloak =
  attachment GreyCloak Cards.greyCloak
    & attachTo
      ( AttachToCharacterMatching
          (HeroMatchAny [HeroWithSphere Spirit, HeroWithTrait Scout])
          (AllyMatchAny [AllyWithSphere Spirit, AllyWithTrait Scout])
      )

nonUniqueStaged :: EnemyMatcher
nonUniqueStaged = EnemyInStagingArea <> not_ UniqueEnemy

-- | Encounter Action: Exhaust attached character and discard Grey Cloak to choose a
-- non-unique enemy in the staging area. That enemy does not make engagement checks against
-- you this phase.
instance HasAbilities GreyCloak where
  getAbilities (GreyCloak a) =
    [ restricted a 1 (DuringPhase EncounterPhase <> exists nonUniqueStaged) $ ActionAbility (ExhaustCost host)
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage GreyCloak where
  runMessage msg s@(GreyCloak a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      discard a
      enemies <- select nonUniqueStaged
      chooseOrRunOneM pid do
        questionLabeled "greyCloak.enemy"
        causedBy a
        targets enemies \eid -> phaseModifier a eid (NoEngagementChecksAgainst pid)
      pure s
    _ -> GreyCloak <$> runMessage msg a
