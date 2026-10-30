module LordOfTheRings.Ally.Cards.SamGamgee (samGamgee) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (Placement (Unplaced))
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype SamGamgee = SamGamgee AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

samGamgee :: AllyCard SamGamgee
samGamgee = ally SamGamgee Cards.samGamgee

-- | Reduce the cost to play Sam Gamgee by 2 if you control Frodo Baggins.
instance HasModifiersFor SamGamgee where
  getModifiersFor (SamGamgee a) = when (a.placement == Unplaced) do
    frodo <- selectAny (HeroControlledBy (PlayerWithId a.owner) <> HeroWithTitle "Frodo Baggins")
    when frodo $ modified a (CardIdTarget a.allyCardId) [CostModifier (-2)]

-- | Response: After a player raises his threat, that player may spend 1 spirit resource to
-- ready Sam Gamgee. He gets +1 willpower, +1 attack and +1 defense until the end of the
-- round. (Limit once per round.)
instance HasAbilities SamGamgee where
  getAbilities (SamGamgee a) =
    [ anyPlayer
        $ limited (GroupLimit PerRound 1)
        $ mkAbility a 1
        $ ResponseAbility (ThreatRaisedWindow After ActivePlayer) (SpendResourcesCost Spirit 1)
    ]

instance RunMessage SamGamgee where
  runMessage msg s@(SamGamgee a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ready a
      roundModifiers a a [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1]
      pure s
    _ -> SamGamgee <$> runMessage msg a
