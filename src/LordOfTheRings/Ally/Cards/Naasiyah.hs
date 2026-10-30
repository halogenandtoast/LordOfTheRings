module LordOfTheRings.Ally.Cards.Naasiyah (naasiyah) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))

-- | The Stormcaller Elite's Na'asiyah, objective-ally side faceup, under the last player's
-- control: flipped, she is the enemy of "LordOfTheRings.Enemy.Cards.Naasiyah".
newtype Naasiyah = Naasiyah AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

naasiyah :: AllyCard Naasiyah
naasiyah = ally Naasiyah Cards.naasiyahObjective

resources :: AllyAttrs -> Int
resources = countTokens Resource . allyTokens

-- | Action: While Na'asiyah is attacking or defending, spend 1 resource from her to give
-- her +2 attack or +2 defense for this attack.
-- Action: Spend 1 resource from a hero you control to place 1 resource on Na'asiyah.
-- (Limit once per round.)
-- If Na'asiyah leaves play, the players lose.
instance HasAbilities Naasiyah where
  getAbilities (Naasiyah a) =
    [ restricted a 1 (exists $ AllyWithId a.id <> AllyMatchAny [AttackingAlly, DefendingAlly]) $ ActionAbility Free
    | resources a > 0
    ]
      <> [ limited (GroupLimit PerRound 1) $ restricted a 2 (exists $ HeroControlledBy ActivePlayer <> HeroWithResources) $ ActionAbility Free
         , mkAbility a 3 $ forced $ LeftPlayWindow After (toTarget a)
         ]

instance RunMessage Naasiyah where
  runMessage msg s@(Naasiyah a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      removeTokens a a Resource 1
      chooseOneM pid do
        questionLabeled "naasiyah.choose"
        causedBy a
        labeled "naasiyah.attack" $ attackModifier a a (AttackModifier 2)
        labeled "naasiyah.defense" $ attackModifier a a (DefenseModifier 2)
      pure s
    UseCardAbility pid (isSource a -> True) 2 _ -> do
      spendResourceFromAHero pid a
      placeResources a a 1
      pure s
    UseCardAbility _ (isSource a -> True) 3 _ -> do
      gameOver Lost
      pure s
    _ -> Naasiyah <$> runMessage msg a
