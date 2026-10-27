module LordOfTheRings.Ally.Cards.RiderOfTheMark (riderOfTheMark) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype RiderOfTheMark = RiderOfTheMark AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

riderOfTheMark :: AllyCard RiderOfTheMark
riderOfTheMark = ally RiderOfTheMark Cards.riderOfTheMark

-- | Action: Spend 1 spirit resource to give control of Rider of the Mark to another
-- player. (Limit once per round.)
-- Response: After Rider of the Mark changes control, discard a shadow card dealt to an
-- enemy you are engaged with.
instance HasAbilities RiderOfTheMark where
  getAbilities (RiderOfTheMark a) =
    concat
      [ [ limited (GroupLimit PerRound 1)
            $ restricted a 1 (PlayerExists $ NotPlayer controller)
            $ ActionAbility (SphereResourceCost pid Spirit 1)
        , restricted
            a
            2
            ( AnyCriteria
                [exists (EnemyEngagedWith controller <> EnemyWithFacedownShadow), exists (EnemyEngagedWith controller <> EnemyWithRevealedShadow)]
            )
            $ response
            $ ChangedControlWindow After (toTarget a)
        ]
      | Just pid <- [placementController a.placement]
      , let controller = PlayerWithId pid
      ]

instance RunMessage RiderOfTheMark where
  runMessage msg s@(RiderOfTheMark a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      others <- select (NotPlayer $ PlayerWithId pid)
      chooseOneM pid do
        questionLabeled "riderOfTheMark.giveControl"
        causedBy a
        targets others \p -> takeControl a.id p
      pure s
    UseCardAbility pid (isSource a -> True) 2 _ -> do
      enemies <- traverse getEnemy =<< select (EnemyEngagedWith $ PlayerWithId pid)
      chooseOneM pid do
        questionLabeled "riderOfTheMark.discardShadow"
        causedBy a
        for_ enemies \e -> targets e.shadowCards \c -> discardShadowCard e.id c
      pure s
    _ -> RiderOfTheMark <$> runMessage msg a
