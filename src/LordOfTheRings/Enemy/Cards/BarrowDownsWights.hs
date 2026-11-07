module LordOfTheRings.Enemy.Cards.BarrowDownsWights (barrowWight, coldWight, darkWight) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype BarrowDownsWights = BarrowDownsWights EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

barrowWight, coldWight, darkWight :: EnemyCard BarrowDownsWights
barrowWight = enemy BarrowDownsWights Cards.barrowWight
coldWight = enemy BarrowDownsWights Cards.coldWight
darkWight = enemy BarrowDownsWights Cards.darkWight

instance HasModifiersFor BarrowDownsWights where
  getModifiersFor (BarrowDownsWights a) = modifySelf a $
    if toCardCode a == toCardCode Cards.barrowWight then [EngagedPlayerModifier CannotReduceThreat]
    else if toCardCode a == toCardCode Cards.coldWight then [EngagedPlayerModifier CannotDrawCardsFromCardEffects]
    else [EngagedHeroesModifier CannotGainResourcesFromCardEffects]

instance HasAbilities BarrowDownsWights where
  getAbilities (BarrowDownsWights a) =
    [mkAbility a 1 $ forced $ EnemyAttacksAndDestroysCharacterWindow After a.id]

instance RunMessage BarrowDownsWights where
  runMessage msg e@(BarrowDownsWights a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      -- Record the controller when each character died: a Sentinel's controller
      -- is not necessarily the player whose enemy is attacking (FAQ 1.52).
      destroyed <- attackDestroyed a.id
      for_ (mapMaybe (.controller) destroyed) \pid -> do
        if toCardCode a == toCardCode Cards.barrowWight then encounterRaisesThreat pid 2
        else if toCardCode a == toCardCode Cards.coldWight then discardRandomFromHand pid 1
        else do
          heroes <- select $ HeroControlledBy (PlayerWithId pid)
          for_ heroes \hid -> do
            resources <- (.resources) <$> getHero hid
            removeTokens a hid Resource resources
      pure e
    _ -> BarrowDownsWights <$> runMessage msg a
