module LordOfTheRings.Treachery.Cards.StalkedByDunlendings (stalkedByDunlendings) where

import LordOfTheRings.Card (CardDef (..), CardType (EnemyType), toCardDef)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype StalkedByDunlendings = StalkedByDunlendings TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

stalkedByDunlendings :: TreacheryCard StalkedByDunlendings
stalkedByDunlendings = treachery StalkedByDunlendings Cards.stalkedByDunlendings

-- | When Revealed: Each player draws 1 card. Until the end of the round, add X threat to
-- the total threat in the staging area. X is the number of cards in the first player's
-- hand.
-- Shadow: Put the topmost enemy in the discard pile into play engaged with you and deal it
-- a shadow card.
instance RunMessage StalkedByDunlendings where
  runMessage msg t@(StalkedByDunlendings attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> drawCards attrs p 1
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      x <- length . (.hand) <$> (getPlayer =<< getLead)
      roundModifier attrs GameTarget (StagingThreatBonus x)
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      discardPile <- getEncounterDiscard
      for_ (find ((== EnemyType) . cdCardType . toCardDef) discardPile) \card -> do
        removeFromEncounterDiscard card
        dealShadowCard =<< createEnemyEngaged pid card
      pure t
    _ -> StalkedByDunlendings <$> runMessage msg attrs
