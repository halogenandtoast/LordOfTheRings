module LordOfTheRings.Ally.Cards.ElladanLore (elladanLore) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (Placement (Unplaced), placementController)
import LordOfTheRings.Trait (Trait (Orc))

newtype ElladanLore = ElladanLore AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

elladanLore :: AllyCard ElladanLore
elladanLore = ally ElladanLore Cards.elladanLore

-- | While Elrohir is in play, Elladan gets +2 attack.
instance HasModifiersFor ElladanLore where
  getModifiersFor (ElladanLore a) = unless (a.placement == Unplaced) do
    elrohir <- orM [selectAny (HeroWithTitle "Elrohir"), selectAny (AllyWithTitle "Elrohir")]
    when elrohir $ modifySelf a [AttackModifier 2]

-- | Response: After you optionally engage an Orc enemy, ready Elladan.
instance HasAbilities ElladanLore where
  getAbilities (ElladanLore a) =
    [ restricted a 1 (exists $ AllyWithId a.id <> ExhaustedAlly)
        $ response
        $ PlayerOptionallyEngagedMatchingWindow After pid (EnemyWithTrait Orc)
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage ElladanLore where
  runMessage msg s@(ElladanLore a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ready a
      pure s
    _ -> ElladanLore <$> runMessage msg a
