module LordOfTheRings.Treachery.Cards.CutOffOsgiliath (cutOffOsgiliath) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType))

newtype CutOffOsgiliath = CutOffOsgiliath TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

cutOffOsgiliath :: TreacheryCard CutOffOsgiliath
cutOffOsgiliath = treachery CutOffOsgiliath Cards.cutOffOsgiliath

instance RunMessage CutOffOsgiliath where
  runMessage msg t@(CutOffOsgiliath attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        allies <- filter ((== AllyType) . (.cardType)) . (.hand) <$> getPlayer pid
        traverse_ (discardFromHand pid) allies
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      allies <- filter ((== AllyType) . (.cardType)) . (.hand) <$> getPlayer pid
      let needed = if null defenders then 2 else 1
      chooseOneM pid do
        questionLabeled "cutOffOsgiliath.shadow"
        causedBy attrs
        when (length allies >= needed) $ labeled "cutOffOsgiliath.discard" $ doStep needed msg
        labeled "cutOffOsgiliath.attack" $ enemyGetsAttack attrs eid 3
      pure t
    DoStep n shadow@(ResolveShadowEffect cid _ pid _) | cid == attrs.cardId, n > 0 -> do
      allies <- filter ((== AllyType) . (.cardType)) . (.hand) <$> getPlayer pid
      chooseOrRunOneM pid do
        questionLabeled "cutOffOsgiliath.discardAlly"
        causedBy attrs
        targets allies \card -> do
          discardFromHand pid card
          when (n > 1) $ doStep (n - 1) shadow
      pure t
    _ -> CutOffOsgiliath <$> runMessage msg attrs
