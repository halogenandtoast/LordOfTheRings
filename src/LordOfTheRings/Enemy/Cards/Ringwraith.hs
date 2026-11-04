module LordOfTheRings.Enemy.Cards.Ringwraith (ringwraith) where

import LordOfTheRings.Card (EncounterCard (MkEncounterCard), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Morgul))

newtype Ringwraith = Ringwraith EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

ringwraith :: EnemyCard Ringwraith
ringwraith = enemy Ringwraith Cards.ringwraith

-- | Cannot have non-Morgul attachments.
instance HasModifiersFor Ringwraith where
  getModifiersFor (Ringwraith a) = modifySelf a [CannotHaveAttachmentsWithoutTrait Morgul]

-- | Forced: When Ringwraith attacks, deal it an additional shadow card if The One Ring is
-- exhausted.
instance HasAbilities Ringwraith where
  getAbilities (Ringwraith a) = [restricted a 1 TheOneRingExhausted $ forced $ EnemyAttacksWindow When a.id]

-- | Shadow: Shuffle this card into the encounter deck.
instance RunMessage Ringwraith where
  runMessage msg e@(Ringwraith attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      dealShadowCard attrs.id
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      shuffleShadowCardIntoEncounterDeck eid (MkEncounterCard cid (toCardCode attrs))
      pure e
    _ -> Ringwraith <$> runMessage msg attrs
