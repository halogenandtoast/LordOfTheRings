module LordOfTheRings.Treachery.Cards.TraitorOfManyColors (traitorOfManyColors) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Player.Types (discardableHand)

newtype TraitorOfManyColors = TraitorOfManyColors TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

traitorOfManyColors :: TreacheryCard TraitorOfManyColors
traitorOfManyColors = treachery TraitorOfManyColors Cards.traitorOfManyColors

instance RunMessage TraitorOfManyColors where
  runMessage msg t@(TraitorOfManyColors a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      you <- getRevealer
      enemies <- select (EnemyWithCardCode $ toCardCode Enemies.sarumanTheRoadToIsengard)
      for_ enemies (immediateAttack you)
      pure t
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      hand <- discardableHand <$> getPlayer pid
      barred <- elem CannotDiscardFromHand <$> getModifiers (PlayerTarget pid)
      enemyMods <- getModifiers eid
      chooseOrRunOneM pid do
        questionLabeled "traitorOfManyColors.choose"
        causedBy a
        when (notNull hand && not barred) $ labeled "traitorOfManyColors.discard" $ discardRandomFromHand pid 1
        when (CannotAttack `notElem` enemyMods && CannotAttackPlayer pid `notElem` enemyMods) $
          labeled "traitorOfManyColors.attack" $ additionalAttack pid eid
      pure t
    _ -> TraitorOfManyColors <$> runMessage msg a
