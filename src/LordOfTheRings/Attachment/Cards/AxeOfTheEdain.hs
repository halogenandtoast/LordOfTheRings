module LordOfTheRings.Attachment.Cards.AxeOfTheEdain (axeOfTheEdain) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype AxeOfTheEdain = AxeOfTheEdain AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | An Ered Mithrin treasure. Attach to a character. Restricted, on its card def.
axeOfTheEdain :: AttachmentCard AxeOfTheEdain
axeOfTheEdain = attachment AxeOfTheEdain Cards.axeOfTheEdain & attachTo AttachToCharacter

-- | Attached character gets +2 [attack].
instance HasModifiersFor AxeOfTheEdain where
  getModifiersFor (AxeOfTheEdain a) = withHost a \host -> modified a host [AttackModifier 2]

nonUniqueEngaged :: PlayerId -> EnemyMatcher
nonUniqueEngaged pid = EnemyEngagedWith (PlayerWithId pid) <> NotEnemy UniqueEnemy <> EnemyNotImmuneToPlayerCards

-- | Response: After you play Axe of the Edain, deal 2 damage to a non-unique enemy engaged
-- with you.
instance HasAbilities AxeOfTheEdain where
  getAbilities (AxeOfTheEdain a) =
    [restricted a 1 (exists $ nonUniqueEngaged a.controller) $ response $ PlayedFromHandWindow After (CardIdTarget a.cardId)]

instance RunMessage AxeOfTheEdain where
  runMessage msg s@(AxeOfTheEdain a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select (nonUniqueEngaged pid)
      chooseOrRunOneM pid do
        questionLabeled "chooseEnemy"
        causedBy a
        targets enemies \e -> dealDamage a e 2
      pure s
    _ -> AxeOfTheEdain <$> runMessage msg a
