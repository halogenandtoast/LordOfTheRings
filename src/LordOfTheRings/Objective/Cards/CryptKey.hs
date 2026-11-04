module LordOfTheRings.Objective.Cards.CryptKey (cryptKey) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheGhostOfFramsburg.Helpers (lootObjective)

newtype CryptKey = CryptKey ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

cryptKey :: ObjectiveCard CryptKey
cryptKey = objective CryptKey Cards.cryptKey

-- | Loot. If unattached, the first player claims Crypt Key and attaches it to a hero he
-- controls.
instance HasModifiersFor CryptKey where
  getModifiersFor (CryptKey a) = lootObjective a

-- | Response: When the players travel to a Discover location, exhaust Crypt Key to either
-- increase or decrease its Discover value by 1 before resolving it.
instance HasAbilities CryptKey where
  getAbilities (CryptKey a) =
    [ mkAbility a 1 $ ResponseAbility (TraveledToWindow When LocationWithDiscover) (exhaustCost a)
    | AttachedTo _ <- [a.placement]
    , not a.exhausted
    ]

-- | The change lasts until the keyword resolves, which spends it.
instance RunMessage CryptKey where
  runMessage msg o@(CryptKey attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 ws -> do
      for_ [lid | Window When (Traveled lid) <- ws] \lid -> chooseOneM pid do
        questionLabeled "cryptKey.choose"
        causedBy attrs
        labeled "cryptKey.increase" $ createEffect attrs lid [DiscoverValueModifier 1] UntilSpent
        labeled "cryptKey.decrease" $ createEffect attrs lid [DiscoverValueModifier (-1)] UntilSpent
      pure o
    _ -> CryptKey <$> runMessage msg attrs
