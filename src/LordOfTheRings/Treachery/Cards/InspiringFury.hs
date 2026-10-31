module LordOfTheRings.Treachery.Cards.InspiringFury (inspiringFury) where

import LordOfTheRings.Card (EncounterCard (MkEncounterCard), toCardCode)
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype InspiringFury = InspiringFury TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

inspiringFury :: TreacheryCard InspiringFury
inspiringFury = treachery InspiringFury Cards.inspiringFury

-- | Surge.
-- When Revealed: Deal Inspiring Fury to the enemy with the highest attack, as a shadow
-- card: discarded as it resolves, it leaves the discard pile for that enemy.
-- Shadow: Shuffle the encounter discard pile into the encounter deck and deal attacking
-- enemy 3 additional shadow cards. During this attack, shadow effects cannot be canceled.
-- ponytail: no encounter card effect can be canceled during this attack
instance RunMessage InspiringFury where
  runMessage msg t@(InspiringFury attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      enemies <- traverse (\e -> (e,) <$> getEnemyAttack e) =<< select AnyEnemy
      let highest = foldr (max . snd) 0 enemies
          card = MkEncounterCard attrs.cardId (toCardCode attrs)
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "inspiringFury.enemy"
        causedBy attrs
        targets [e | (e, x) <- enemies, x == highest] \e -> do
          discard attrs
          removeFromEncounterDiscard card
          whileInPlay (EnemyTarget e) $ push $ AttachShadowCard e card
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      attackModifier (CardIdSource cid) GameTarget CannotCancelEncounterEffects
      dealShadowCardsDuringAttack eid pid defenders 3
      pure t
    _ -> InspiringFury <$> runMessage msg attrs
