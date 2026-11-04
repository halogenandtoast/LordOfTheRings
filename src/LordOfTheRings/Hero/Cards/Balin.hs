module LordOfTheRings.Hero.Cards.Balin (balin) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype Balin = Balin HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

balin :: HeroCard Balin
balin = hero Balin Cards.balin

-- | Response: Pay 1 resource from Balin's resource pool to cancel a shadow effect just
-- triggered during an attack. Then, deal the attacking enemy another shadow card. (Limit
-- once per attack.)
instance HasAbilities Balin where
  getAbilities (Balin a) =
    [ limited (PlayerLimit PerAttack 1)
        $ restricted a 1 shadowEffectsCancelable
        $ ResponseAbility (ShadowEffectWindow When) (ResourceCost (toTarget a) 1)
    ]

-- | The new shadow card turns face up and resolves in this attack, as the shadow cards dealt
-- during an attack do: against the attack's defenders, the player it is made against the
-- one engaged with the enemy.
instance RunMessage Balin where
  runMessage msg h@(Balin attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      cancelShadowEffects windows
      for_ (take 1 [eid | Window _ (ShadowEffectTriggered _ eid) <- windows]) \eid -> do
        engaged <- placementController . (.placement) <$> getEnemy eid
        pid <- maybe getLead pure engaged
        defenders <- getDefendersAgainst eid
        dealShadowCardsDuringAttack eid pid defenders 1
      pure h
    _ -> Balin <$> runMessage msg attrs
