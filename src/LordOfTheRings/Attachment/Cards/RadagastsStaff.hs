module LordOfTheRings.Attachment.Cards.RadagastsStaff (radagastsStaff) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Trait (Trait (Creature))

newtype RadagastsStaff = RadagastsStaff AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to Radagast.
radagastsStaff :: AttachmentCard RadagastsStaff
radagastsStaff =
  attachment RadagastsStaff Cards.radagastsStaff
    & attachTo (AttachToCharacterMatching (HeroWithTitle "Radagast") (AllyWithTitle "Radagast"))

-- | Action: Exhaust Radagast's Staff to choose one: Reduce the cost of the next Creature
-- ally you play this phase by 2, ready a Creature ally, or return a non-unique Creature
-- enemy engaged with you to the staging area.
instance HasAbilities RadagastsStaff where
  getAbilities (RadagastsStaff a) = [mkAbility a 1 $ ActionAbility (exhaustCost a) | AttachedTo _ <- [a.placement]]

instance RunMessage RadagastsStaff where
  runMessage msg s@(RadagastsStaff a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      allies <- select (ExhaustedAlly <> AllyWithTrait Creature)
      enemies <- select (EnemyEngagedWith (PlayerWithId pid) <> EnemyWithTrait Creature <> not_ UniqueEnemy)
      chooseOneM pid do
        questionLabeled "radagastsStaff.choose"
        causedBy a
        labeled "radagastsStaff.reduce"
          $ phaseModifier a (PlayerTarget pid) (LowerNextCostOf (CardWithType AllyType <> CardWithTrait Creature) 2)
        unless (null allies) $ labeled "radagastsStaff.ready" do
          chooseOrRunOneM pid do
            questionLabeled "radagastsStaff.ally"
            causedBy a
            targets allies ready
        unless (null enemies) $ labeled "radagastsStaff.return" do
          chooseOrRunOneM pid do
            questionLabeled "radagastsStaff.enemy"
            causedBy a
            targets enemies \eid -> placeEnemy eid StagingArea
      pure s
    _ -> RadagastsStaff <$> runMessage msg a
