module LordOfTheRings.Enemy.Cards.PrancingPonySuspects
  (toddTheTroll, susieTheSpider, wolfPeltJake, johnnyGoblinFingers, oldOrcEyes) where

import LordOfTheRings.Card (cdShadowEffect, toCardDef, toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Trait (Trait (Brigand))
import LordOfTheRings.Placement (Placement (StagingArea))

newtype PrancingPonySuspect = PrancingPonySuspect EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

toddTheTroll, susieTheSpider, wolfPeltJake, johnnyGoblinFingers, oldOrcEyes :: EnemyCard PrancingPonySuspect
toddTheTroll = enemy PrancingPonySuspect Cards.toddTheTroll
susieTheSpider = enemy PrancingPonySuspect Cards.susieTheSpider
wolfPeltJake = enemy PrancingPonySuspect Cards.wolfPeltJake
johnnyGoblinFingers = enemy PrancingPonySuspect Cards.johnnyGoblinFingers
oldOrcEyes = enemy PrancingPonySuspect Cards.oldOrcEyes

instance HasModifiersFor PrancingPonySuspect where
  getModifiersFor (PrancingPonySuspect a) = do
    modifySelf a [CannotHaveAttachments]
    when (toCardCode a == toCardCode Cards.toddTheTroll) $ modifySelf a [ExcessDamageToThreat]
    when (toCardCode a == toCardCode Cards.susieTheSpider) do
      brigands <- select $ EnemyWithTrait Brigand
      modifySelf a $ ThreatModifier (length brigands) : [CannotTakeDamage | any (/= a.id) brigands]

instance HasAbilities PrancingPonySuspect where
  getAbilities (PrancingPonySuspect a)
    | toCardCode a == toCardCode Cards.wolfPeltJake = [mkAbility a 1 $ forced $ EnemyAttacksAndDestroysCharacterWindow After a.id]
    | toCardCode a == toCardCode Cards.johnnyGoblinFingers = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]
    | otherwise = []

instance RunMessage PrancingPonySuspect where
  runMessage msg e@(PrancingPonySuspect a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      if toCardCode a == toCardCode Cards.wolfPeltJake
        then whenM (notNull <$> attackDestroyedAllies a.id) $ placeEnemy a.id StagingArea
        else for_ [pid | Window _ (EnemyAttacking eid pid) <- ws, eid == a.id] $ \pid ->
          discardRandomFromHandWithResult a pid 1
      pure e
    DiscardedRandomFromHand (isSource a -> True) pid cards -> do
      encounterRaisesThreat pid (sum $ map printedCostOf cards)
      pure e
    CheckWindows ws | toCardCode a == toCardCode Cards.oldOrcEyes -> do
      -- As for Wargs, a blank shadow counts when it turns faceup, even if it
      -- is discarded afterward (Nate French, ALeP FAQ 3.003; Caleb 3.274 #3).
      -- Several blank shadows still cause only one additional attack. Its old
      -- shadows are discarded and no fresh shadow is dealt (printed card; FAQ 1.42).
      mods <- getModifiers a
      let marker = UsedThisAttack (toCardCode a)
          blank = [pid | (card, pid, _) <- revealedShadowCards a ws, not $ cdShadowEffect $ toCardDef card]
      when (marker `notElem` mods) $ for_ (take 1 blank) $ \pid -> do
        createEffect a a [marker] (UntilEnemyAttackEnds a.id)
        additionalAttackWithoutShadow pid a.id
      PrancingPonySuspect <$> runMessage msg a
    _ -> PrancingPonySuspect <$> runMessage msg a
