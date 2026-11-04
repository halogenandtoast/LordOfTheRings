module LordOfTheRings.Ally.Cards.Calaerion (calaerion, calaerionUpgraded) where

import LordOfTheRings.Ally.Fleet (runShip)
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Sphere (Sphere (Neutral))

newtype Calaerion = Calaerion AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Dream-chaser campaign, put into play under the player with the
-- Dream-chaser.
calaerion :: AllyCard Calaerion
calaerion = ally Calaerion Cards.calaerion

calaerionUpgraded :: AllyCard Calaerion
calaerionUpgraded = ally Calaerion Cards.calaerionUpgraded

upgraded :: AllyAttrs -> Bool
upgraded a = toCardCode a == toCardCode Cards.calaerionUpgraded

{- | Action: Spend 3 resources and exhaust Calaerion to ready the Dream-chaser and give it
+1 willpower, +1 attack, and +1 defense until the end of the phase.
Upgraded: Action: Spend 2 resources and exhaust Calaerion to ready another ally and give it
+1 willpower, +1 attack, and +1 defense until the end of the round.
-}
instance HasAbilities Calaerion where
  getAbilities (Calaerion a) =
    [ restricted a 1 (ResourcesOfSphere ActivePlayer Neutral cost) $ ActionAbility (exhaustCost a)
    ]
   where
    cost = if upgraded a then 2 else 3

-- | When Calaerion leaves play, remove him from the game.
instance RunMessage Calaerion where
  runMessage msg s@(Calaerion a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      let up = upgraded a
      replicateM_ (if up then 2 else 3) $ spendResourceFromAHero pid a
      targets' <- if up then select (not_ (AllyWithId a.id)) else select (AllyWithTitle "Dream-chaser")
      chooseOrRunOneM pid do
        questionLabeled "calaerion.ready"
        causedBy a
        targets targets' \t -> do
          ready t
          (if up then roundModifiers else phaseModifiers) a t [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1]
      pure s
    _ -> Calaerion <$> runShip (pure ()) msg a
