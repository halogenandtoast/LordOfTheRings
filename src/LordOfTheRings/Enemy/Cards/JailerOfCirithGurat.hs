module LordOfTheRings.Enemy.Cards.JailerOfCirithGurat (jailerOfCirithGurat) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Staging (getEnemyThreat)

newtype JailerOfCirithGurat = JailerOfCirithGurat EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

jailerOfCirithGurat :: EnemyCard JailerOfCirithGurat
jailerOfCirithGurat = enemy JailerOfCirithGurat Cards.jailerOfCirithGurat

-- | Forced: After Jailer of Cirith Gurat engages you, attach an ally you control to it.
instance HasAbilities JailerOfCirithGurat where
  getAbilities (JailerOfCirithGurat a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: If attacking enemy's threat is greater than defending ally's willpower, attach
-- defending ally to attacking enemy. Then, cancel this attack.
instance RunMessage JailerOfCirithGurat where
  runMessage msg e@(JailerOfCirithGurat attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        allies <- select (AllyControlledBy $ PlayerWithId pid)
        chooseOrRunOneM pid do
          questionLabeled "jailerOfCirithGurat.attach"
          causedBy attrs
          targets allies \al -> guardCharacterWith al attrs
      pure e
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      threat <- getEnemyThreat eid
      captor <- getEnemy eid
      for_ [d | d@(AllyTarget _) <- defenders] \d -> do
        willpower <- getWillpower d
        when (threat > willpower) do
          guardCharacterWith d captor
          cancelAttack eid
      pure e
    _ -> JailerOfCirithGurat <$> runMessage msg attrs
