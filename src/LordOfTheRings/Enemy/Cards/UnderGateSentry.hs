module LordOfTheRings.Enemy.Cards.UnderGateSentry (underGateSentry) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers, immuneToSource)
import LordOfTheRings.Helpers.Window (exhaustableCharacters)
import LordOfTheRings.Player.Types (discardableHand)

newtype UnderGateSentry = UnderGateSentry EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

underGateSentry :: EnemyCard UnderGateSentry
underGateSentry = enemy UnderGateSentry Cards.underGateSentry

instance HasAbilities UnderGateSentry where
  getAbilities (UnderGateSentry a) = [mkAbility a 1 $ forced $ EnemyAttacksAndDestroysYourCharacterWindow After a.id]

instance RunMessage UnderGateSentry where
  runMessage msg e@(UnderGateSentry a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ (take 1 [pid | Window _ (EnemyAttacking eid pid) <- ws, eid == a.id]) \pid -> discardRandomFromHand pid 1
      pure e
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      hand <- discardableHand <$> getPlayer pid
      barred <- elem CannotDiscardFromHand <$> getModifiers (PlayerTarget pid)
      available <- filterM (fmap not . immuneToSource (toSource a)) =<< exhaustableCharacters (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "underGateSentry.choose"
        causedBy a
        when (notNull hand && not barred) $ labeled "underGateSentry.discard" $ discardRandomFromHand pid 1
        unless (null available) $ labeled "underGateSentry.exhaust" $ chooseAndExhaust a pid 1
      pure e
    _ -> UnderGateSentry <$> runMessage msg a
