module LordOfTheRings.Enemy.Cards.PrancingPonyBrigands (lawlessRuffian, greenwayFootpad, chetwoodOutlaw) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype LawlessRuffian = LawlessRuffian EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

lawlessRuffian :: EnemyCard LawlessRuffian
lawlessRuffian = enemy LawlessRuffian Cards.lawlessRuffian

instance HasModifiersFor LawlessRuffian where
  getModifiersFor (LawlessRuffian a) = case a.placement of
    EngagedWith pid -> whenM ((>= 35) <$> getThreat pid) $ modifySelf a [AttackModifier 2]
    _ -> pure ()

instance RunMessage LawlessRuffian where
  runMessage msg e@(LawlessRuffian a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> e <$ additionalAttack pid eid
    _ -> LawlessRuffian <$> runMessage msg a

newtype GreenwayFootpad = GreenwayFootpad EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

greenwayFootpad :: EnemyCard GreenwayFootpad
greenwayFootpad = enemy GreenwayFootpad Cards.greenwayFootpad

instance RunMessage GreenwayFootpad where
  runMessage msg e@(GreenwayFootpad a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "prancingPony.footpad"
        causedBy a
        labeled "prancingPony.raiseEachThreat2" $ eachPlayer $ \pid -> encounterRaisesThreat pid 2
        labeled "prancingPony.immediateAttack" $ immediateAttack lead a.id
      pure e
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      threat <- getThreat pid
      enemyGetsAttack a eid (if threat >= 35 then 2 else 1)
      pure e
    _ -> GreenwayFootpad <$> runMessage msg a

newtype ChetwoodOutlaw = ChetwoodOutlaw EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

chetwoodOutlaw :: EnemyCard ChetwoodOutlaw
chetwoodOutlaw = enemy ChetwoodOutlaw Cards.chetwoodOutlaw

instance HasAbilities ChetwoodOutlaw where
  getAbilities (ChetwoodOutlaw a) = [mkAbility a 1 $ forced $ EnemyOptionallyEngagedWindow After a.id]

instance RunMessage ChetwoodOutlaw where
  runMessage msg e@(ChetwoodOutlaw a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [pid | Window _ (OptionallyEngaged eid pid) <- ws, eid == a.id] $ \pid -> encounterRaisesThreat pid 4
      pure e
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      chooseOneM pid do
        questionLabeled "prancingPony.outlawShadow"
        causedBy a
        labeled "prancingPony.raiseThreat2" $ encounterRaisesThreat pid 2
        labeled "prancingPony.returnAfterAttack" $ doStep 1 msg
      pure e
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == a.cardId -> e <$ returnToStagingAfterAttack eid
    _ -> ChetwoodOutlaw <$> runMessage msg a
