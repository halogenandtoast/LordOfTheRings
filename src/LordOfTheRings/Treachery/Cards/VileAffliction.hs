module LordOfTheRings.Treachery.Cards.VileAffliction (vileAffliction) where

import LordOfTheRings.Card (Card (PlayerCard), CardCost (StaticCost), cdCost, toCardCode, toCardDef)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype VileAffliction = VileAffliction TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

vileAffliction :: TreacheryCard VileAffliction
vileAffliction = treachery VileAffliction Cards.vileAffliction

-- | When Revealed: Each player must distribute damage among questing characters he controls
-- equal to the printed cost of the highest cost card in that player's discard pile. A card
-- with no printed number (X, or a hero's) costs 0.
-- Shadow: Attacking enemy gets +1 attack. If this attack destroys an ally, search your deck
-- for another copy of that ally and discard it, if able.
instance RunMessage VileAffliction where
  runMessage msg t@(VileAffliction attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        discards <- (.discard) <$> getPlayer p
        let x = foldr (max . printedCost) 0 discards
        questing <-
          selectCharacters
            (HeroControlledBy (PlayerWithId p) <> CommittedHero)
            (AllyControlledBy (PlayerWithId p) <> CommittedAlly)
        when (x > 0) $ dealDamageDividedAmong attrs p questing x
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == attrs.cardId -> do
      allies <- map (toCardCode . (.card)) <$> attackDestroyedAllies eid
      deck <- (.deck) <$> getPlayer pid
      for_ (ordNub allies) \code ->
        for_ (headMay [pc | PlayerCard pc <- deck, toCardCode pc == code]) (discardFromDeck pid)
      unless (null allies) $ shufflePlayerDeck pid
      pure t
    _ -> VileAffliction <$> runMessage msg attrs
   where
    printedCost c = case cdCost (toCardDef c) of
      Just (StaticCost n) -> n
      _ -> 0
