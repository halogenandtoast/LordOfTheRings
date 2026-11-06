module LordOfTheRings.Enemy.Cards.WainriderCaptain (wainriderCaptain) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMassingAtOsgiliath.Helpers (crossedAnduin)
import LordOfTheRings.Trait (Trait (Scout))

newtype WainriderCaptain = WainriderCaptain EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wainriderCaptain :: EnemyCard WainriderCaptain
wainriderCaptain = enemy WainriderCaptain Cards.wainriderCaptain

instance RunMessage WainriderCaptain where
  runMessage msg e@(WainriderCaptain attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      crossed <- crossedAnduin
      discarded <- getsGame ((.encounterDiscard) . toAttrs . (.scenario))
      let scouts = filter (cardMatches $ CardWithType EnemyType <> CardWithTrait Scout) discarded
      for_ (take (if crossed then 2 else 1) scouts) \card -> do
        removeFromEncounterDiscard card
        addToStaging card
      pure e
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      scout <- selectAny (EnemyWithId eid <> EnemyWithTrait Scout)
      when (null defenders && scout) $ enemyGetsAttack attrs eid 2
      pure e
    _ -> WainriderCaptain <$> runMessage msg attrs
