module LordOfTheRings.Treachery.Cards.WainriderChariot (wainriderChariot) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders.Helpers (placeWainridersDamage)
import LordOfTheRings.Trait (Trait (Easterling, Weapon))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WainriderChariot = WainriderChariot TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, Entity)

wainriderChariot :: TreacheryCard WainriderChariot
wainriderChariot = treachery WainriderChariot Cards.wainriderChariot

-- | While attached, counts as a Weapon attachment with the text: "Attached enemy gets +1
-- Attack. Excess damage dealt by attached enemy must be placed on The Wainriders' stage."
instance HasModifiersFor WainriderChariot where
  getModifiersFor (WainriderChariot a) = case a.placement of
    AttachedTo host@(EnemyTarget _) -> do
      modifySelf a [GainTrait Weapon]
      modified a host [AttackModifier 1]
    _ -> pure ()

instance HasAbilities WainriderChariot where
  getAbilities (WainriderChariot a) = case a.placement of
    AttachedTo (EnemyTarget eid) -> [mkAbility a 1 $ silent $ EnemyDealtExcessDamageWindow After eid]
    _ -> []

-- | Surge.
-- When Revealed: Attach to a non-unique Easterling enemy without a Wainrider Chariot
-- attached, the first player choosing among them.
instance RunMessage WainriderChariot where
  runMessage msg t@(WainriderChariot attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      let anyOfThem = const (pure 0)
      void $ attachToEnemyWith attrs attrs.treacheryId attrs.treacheryCardCode (EnemyWithTrait Easterling <> not_ UniqueEnemy) Lowest anyOfThem (const $ pure ())
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      placeWainridersDamage $ sum [n | Window _ (AttackDealtExcessDamage _ _ n) <- windows]
      pure t
    _ -> WainriderChariot <$> runMessage msg attrs
