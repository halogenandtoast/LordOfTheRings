module LordOfTheRings.Enemy.Cards.DreadnaughtShips (blackSailsDreadnaught, warGalley, seaMonster) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Scenario.Scenarios.TheMumakil.Helpers (remainingHitPoints)

newtype DreadnaughtShip = DreadnaughtShip EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

blackSailsDreadnaught, warGalley, seaMonster :: EnemyCard DreadnaughtShip
blackSailsDreadnaught = enemy DreadnaughtShip Cards.blackSailsDreadnaught
warGalley = enemy DreadnaughtShip Cards.warGalley
seaMonster = enemy DreadnaughtShip Cards.seaMonster

instance HasModifiersFor DreadnaughtShip where
  getModifiersFor (DreadnaughtShip a)
    | toCardCode a == toCardCode Cards.seaMonster = modifySelf a [CanBeFoughtByShipObjectives]
    | otherwise = do
        staged <- selectAny (EnemyWithId a.id <> EnemyInStagingArea)
        modifySelf a $ CannotHaveAttachments
          : [GainKeyword (Archery 3) | staged, toCardCode a == toCardCode Cards.blackSailsDreadnaught]

instance HasAbilities DreadnaughtShip where
  getAbilities (DreadnaughtShip a) =
    [ restricted a 1 (exists $ EnemyWithId a.id <> EnemyInStagingArea)
        $ forced
        $ EnemyEngagedWindow When a.id
    | toCardCode a == toCardCode Cards.warGalley
    ]

instance RunMessage DreadnaughtShip where
  runMessage msg e@(DreadnaughtShip attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid ->
        chooseOneM pid do
          questionLabeled "dreadnaught.warGalley"
          causedBy attrs
          labeled "dreadnaught.raiseFour" $ encounterRaisesThreat pid 4
          labeled "corsairSkiff.boarding" $ roundModifier attrs attrs (BoardingModifier 1)
      pure e
    WhenRevealed (isTarget attrs -> True) | toCardCode attrs == toCardCode Cards.seaMonster -> do
      ships <- (<>) <$> (map toTarget <$> select shipEnemy) <*> (map toTarget <$> select shipObjective)
      remaining <- traverse (\s -> (s,) <$> remainingHitPoints s) ships
      let lowest = minimumMay (map snd remaining)
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "dreadnaught.seaMonster"
        causedBy attrs
        targets [s | (s, n) <- remaining, Just n == lowest] \s -> dealDamage attrs s 4
      pure e
    ResolveShadowEffect cid eid _ _
      | cid == attrs.cardId
      , toCardCode attrs == toCardCode Cards.blackSailsDreadnaught -> do
          whenM (selectAny $ EnemyWithId eid <> shipEnemy) $ returnToStagingAfterAttack eid
          pure e
    _ -> DreadnaughtShip <$> runMessage msg attrs
