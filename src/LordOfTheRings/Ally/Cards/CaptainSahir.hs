module LordOfTheRings.Ally.Cards.CaptainSahir (captainSahir) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))

-- | The Stormcaller Elite's Captain Sahír, objective-ally side faceup, under the first
-- player's control: flipped, he is the enemy of "LordOfTheRings.Enemy.Cards.CaptainSahir".
newtype CaptainSahir = CaptainSahir AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

captainSahir :: AllyCard CaptainSahir
captainSahir = ally CaptainSahir Cards.captainSahirObjective

-- | Captain Sahír gets +1 willpower and +1 attack for each resource on him (max +6).
instance HasModifiersFor CaptainSahir where
  getModifiersFor (CaptainSahir a) = do
    x <- min 6 <$> resourcesHeld a (countTokens Resource $ allyTokens a)
    when (x > 0) $ modifySelf a [WillpowerModifier x, AttackModifier x]

-- | Action: Spend 1 resource from a hero you control to place 1 resource on Captain Sahír.
-- (Limit once per round.)
-- If Captain Sahír leaves play, the players lose.
instance HasAbilities CaptainSahir where
  getAbilities (CaptainSahir a) =
    [ limited (GroupLimit PerRound 1) $ restricted a 1 (exists $ HeroControlledBy ActivePlayer <> HeroWithResources) $ ActionAbility Free
    , mkAbility a 2 $ forced $ LeftPlayWindow After (toTarget a)
    ]

instance RunMessage CaptainSahir where
  runMessage msg s@(CaptainSahir a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      spendResourceFromAHero pid a
      placeResources a a 1
      pure s
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      gameOver Lost
      pure s
    _ -> CaptainSahir <$> runMessage msg a
