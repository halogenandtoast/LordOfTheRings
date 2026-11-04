module LordOfTheRings.Ally.Cards.ElrohirSpirit (elrohirSpirit) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (Placement (Unplaced), placementController)
import LordOfTheRings.Trait (Trait (Orc))

newtype ElrohirSpirit = ElrohirSpirit AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

elrohirSpirit :: AllyCard ElrohirSpirit
elrohirSpirit = ally ElrohirSpirit Cards.elrohirSpirit

-- | While Elladan is in play, Elrohir gets +2 defense.
instance HasModifiersFor ElrohirSpirit where
  getModifiersFor (ElrohirSpirit a) = unless (a.placement == Unplaced) do
    elladan <- orM [selectAny (HeroWithTitle "Elladan"), selectAny (AllyWithTitle "Elladan")]
    when elladan $ modifySelf a [DefenseModifier 2]

-- | Response: After you optionally engage an Orc enemy, ready Elrohir.
instance HasAbilities ElrohirSpirit where
  getAbilities (ElrohirSpirit a) =
    [ restricted a 1 (exists $ AllyWithId a.id <> ExhaustedAlly)
        $ response
        $ PlayerOptionallyEngagedMatchingWindow After pid (EnemyWithTrait Orc)
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage ElrohirSpirit where
  runMessage msg s@(ElrohirSpirit a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ready a
      pure s
    _ -> ElrohirSpirit <$> runMessage msg a
