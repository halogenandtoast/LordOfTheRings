module LordOfTheRings.Treachery.Cards.GollumsRage (gollumsRage) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Card (EncounterCard (MkEncounterCard), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies

newtype GollumsRage = GollumsRage TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gollumsRage :: TreacheryCard GollumsRage
gollumsRage = treachery GollumsRage Cards.gollumsRage

gollum :: EnemyMatcher
gollum = EnemyWithCardCode $ toCardCode Enemies.gollumTheLandOfShadow

instance RunMessage GollumsRage where
  runMessage msg t@(GollumsRage a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      enemies <- select gollum
      for_ (take 1 enemies) \eid -> do
        let card = MkEncounterCard a.cardId (toCardCode a)
        dealRevealedCardAsShadow a eid card
      pure t
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      isGollum <- selectAny (EnemyWithId eid <> gollum)
      when isGollum do
        enemyGetsAttack a eid 1
        additionalAttack pid eid
      pure t
    _ -> GollumsRage <$> runMessage msg a
