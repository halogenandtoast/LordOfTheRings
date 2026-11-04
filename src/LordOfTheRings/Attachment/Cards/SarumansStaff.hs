module LordOfTheRings.Attachment.Cards.SarumansStaff (sarumansStaff) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (EventType))

newtype SarumansStaff = SarumansStaff AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to Saruman. Restricted.
sarumansStaff :: AttachmentCard SarumansStaff
sarumansStaff =
  attachment SarumansStaff Cards.sarumansStaff
    & attachTo (AttachToCharacterMatching (HeroWithTitle "Saruman") (AllyWithTitle "Saruman"))

-- | Action: Exhaust Saruman's Staff to (choose 1): Reduce the X value of the next Doomed X
-- event you play this phase by 2, or search the top 5 cards of your deck for a Doomed event
-- and add it to your hand. Shuffle your deck.
instance HasAbilities SarumansStaff where
  getAbilities (SarumansStaff a) = [mkAbility a 1 $ ActionAbility (exhaustCost a) | AttachedTo _ <- [a.placement]]

instance RunMessage SarumansStaff where
  runMessage msg s@(SarumansStaff a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      chooseOneM pid do
        questionLabeled "sarumansStaff.choose"
        causedBy a
        labeled "sarumansStaff.reduce" $ phaseModifier a (PlayerTarget pid) (LowerNextDoomed 2)
        labeled "sarumansStaff.search" $ searchDeck pid a 5 (CardWithType EventType <> CardWithDoomed) False
      pure s
    _ -> SarumansStaff <$> runMessage msg a
