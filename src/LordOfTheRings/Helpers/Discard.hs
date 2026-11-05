module LordOfTheRings.Helpers.Discard (
  movableDiscardCards,
  protectDiscardedHeroes,
) where

import LordOfTheRings.Card (Card, CardType (HeroType), cdCardType, toCardDef)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Game.Base (Game)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Message
import LordOfTheRings.Modifier (ModifierType (HeroesCannotLeaveDiscard))
import LordOfTheRings.Prelude
import LordOfTheRings.Target

-- | A mixed discard pile still lets its non-hero cards move normally.
movableDiscardCards :: HasGame m => [Card] -> m [Card]
movableDiscardCards cards = do
  locked <- elem HeroesCannotLeaveDiscard <$> getModifiers GameTarget
  pure $ if locked then filter ((/= HeroType) . cdCardType . toCardDef) cards else cards

-- | Enforce the prohibition before a move reaches any entity. Blocking only
-- RemoveFromDiscard would let its following PutHeroIntoPlay create a duplicate.
protectDiscardedHeroes :: HasGame m => Game -> Message -> m (Maybe Message)
protectDiscardedHeroes g msg = do
  locked <- elem HeroesCannotLeaveDiscard <$> getModifiers GameTarget
  let ids = [c.id | p <- toList g.players, c <- p.discard, cdCardType (toCardDef c) == HeroType]
      allowed cid = cid `notElem` ids
      single cid = guard (allowed cid) $> msg
      target = \case CardIdTarget cid -> single cid; _ -> Just msg
      protect = \case
        RemoveFromDiscard _ cid -> single cid
        Reanimate cid _ -> single cid
        PlayCard _ cid -> single cid
        PlayCardInWindow _ cid _ -> single cid
        ReturnFromDiscardToHand _ cid -> single cid
        PutDiscardOnBottomOfDeck _ cid -> single cid
        PutDiscardOnTopOfDeck _ cid -> single cid
        ShuffleFromDiscardIntoDeck _ cid -> single cid
        PutHeroIntoPlay _ c -> single c.id
        PutHeroIntoPlayAs _ _ c -> single c.id
        PutInHand c -> single c.id
        PutOnTopOfDeck _ c -> single c.id
        ShuffleCardIntoDeck _ c -> single c.id
        PutAllyIntoPlayUnder _ c -> single c.id
        PutAttachmentIntoPlay _ c -> single c.id
        SwapHero _ c -> single c.id
        GuardCard c _ -> single c.id
        AttachFacedown _ c -> single c.id
        PlaceUnderneath host cards -> Just $ PlaceUnderneath host (filter (allowed . (.id)) cards)
        AttachPoison host cards -> Just $ AttachPoison host (filter (allowed . (.id)) cards)
        RemoveFromGame t -> target t
        ReturnToHand t -> target t
        ShuffleIntoOwnersDeck t -> target t
        PutOnBottomOfOwnersDeck t -> target t
        AddToVictoryDisplay t -> target t
        _ -> Just msg
  pure $ if locked then protect msg else Just msg
